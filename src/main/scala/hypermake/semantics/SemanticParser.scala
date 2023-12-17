package hypermake.semantics

import better.files._
import cats.instances.all._
import cats.syntax.unorderedTraverse._
import hypermake.collection._
import hypermake.core._
import hypermake.exception._
import hypermake.syntax._
import hypermake.util._

import scala.collection._
import scala.util.matching.Regex

/** A semantic parsing run.
  *
  * @param ctx
  *   Global context
  */
class SemanticParser(implicit val ctx: Context) {

  import ctx._
  import ctx.runtime._

  implicit class DenotationExtension[S, D](s: S)(implicit ssi: Denotation[S, D]) {
    def ! : D = ssi.denotation(s)
  }

  implicit class ContextualDenotationExtension[C, S, D](s: S)(implicit ssi: ContextualDenotation[C, S, D]) {
    def !(ctx: C = ssi.defaultContext) = ssi.denotation(s, ctx)

    def !! : D = ssi.denotation(s, ssi.defaultContext)
  }

  implicit object ParseEnv extends ContextualDenotation[Env, EnvModifier, Env] {
    override def defaultContext = Env(Name("local"))

    def denotation(env: EnvModifier, parent: Env) =
      env.optEnv.fold(parent)(e => Env(Name(e.name)))
  }

  implicit object ParseName extends Denotation[Identifier, Name] {
    def denotation(id: Identifier): Name = Name(id.name)
  }

  implicit object ParseCase extends Denotation[Indices1, Case] {
    def denotation(is: Indices1): Case = Case {
      is.underlying.map { case (id, k) => id.! -> k.key }
    }
  }

  implicit object ParseCaseCube extends Denotation[IndicesN, CaseCube] {
    def denotation(is: IndicesN): CaseCube = CaseCube {
      is.map {
        case (a, Keys(ks)) => a.! -> ks
        case (a, Star())   => a.! -> getAxis(a.!) // *: all values in axis a
      }
    }
  }

  implicit object ParseScript extends Denotation[Verbatim, Script] {
    def denotation(verb: Verbatim): Script = Script(verb.text)
  }

  implicit object ParseValRef
      extends ContextualDenotation[(Map[Name, PointedCube[Value]], Env), ValRef, PointedCube[Value]] {
    def defaultContext = (Map(), Env.local)

    def denotation(r: ValRef, localsEnv: (Map[Name, PointedCube[Value]], Env)): PointedCube[Value] = {
      val name = r.name.!
      val indices = r.indices.!
      val (locals, env) = localsEnv
      val referred = locals
        .get(name) // local variables override global variables
        .orElse(getPackageOpt(name).map(_.output.map(_.on(env)))) // get package output on contextual env
        .getOrElse(getValue(name)) // falls back to global values

      referred.select(indices)
    }
  }

  implicit object ParseStringLiteral extends Denotation[StringLiteral, PointedCube[Value]] {
    def denotation(sl: StringLiteral) = {
      val env = sl.envModifier.!(null) // no env modifier means pure input
      val v = if (env eq null) Value.Pure(sl.value) else Value.Input(sl.value, env)
      PointedCube.Singleton(v)
    }
  }

  implicit object ParseDictLiteral extends Denotation[DictLiteral, PointedCube[Value]] {
    def denotation(dl: DictLiteral) = {
      val DictLiteral(axis, assignments) = dl
      PointedCube.OfNestedMap(
        axis.!,
        assignments.mapValuesE(ParseExpr.denotation(_, (Map(), Env(Name(""))))).pointed(getAxis(axis.!).default)
      )
    }
  }

  implicit object ParseTaskOutputRef1 extends Denotation[TaskOutputRef1, PointedCube[Value.Output]] {
    def denotation(tor: TaskOutputRef1) = {
      val TaskOutputRef1(TaskRef1(task, indices), output) = tor
      getTask(task.!).select(indices.!).map(_.outputs(output.!))
    }
  }

  implicit object ParseTaskOutputRefN extends Denotation[TaskOutputRefN, PointedCube[Value.Multiple]] {
    def denotation(tor: TaskOutputRefN) = {
      val TaskOutputRefN(TaskRefN(taskName, indicesN), output) = tor
      val task = getTask(taskName.!)
      val indices = indicesN.!
      task.curry(indices.vars) map { t =>
        Value.Multiple(
          cases = t.selectMany(indices).map(_.outputs(output.!)),
          env = task.env
        )
      }
    }
  }

  implicit object ParseExpr
      extends ContextualDenotation[(Map[Name, PointedCube[Value]], Env), Expr, PointedCube[Value]] {
    def defaultContext = (Map(), Env.local)

    def denotation(e: Expr, localsEnv: (Map[Name, PointedCube[Value]], Env)) = e match {
      case sl: StringLiteral   => sl.!
      case dl: DictLiteral     => dl.!
      case vr: ValRef          => vr.!(localsEnv)
      case tor: TaskOutputRef1 => tor.!
      case tor: TaskOutputRefN => tor.!
    }
  }

  implicit object ParseFuncCallImpl
      extends ContextualDenotation[(Map[Name, PointedCube[Value]], Env), FuncCallImpl, PointedCube[Script]] {
    def defaultContext = (Map(), Env.local)

    def denotation(impl: FuncCallImpl, localParamsEnv: (Map[Name, PointedCube[Value]], Env)) = {
      val func = getFunc(impl.call.funcName.!)
      val funcArgs = impl.call.inputs
        .map { case (p, (_, a)) => p.! -> a.!(localParamsEnv) }
      func.reify(funcArgs + (func.inputScript -> PointedCube.Singleton(Value.Pure(runtime.nullFile)))).output
    }
  }

  implicit object ParseScriptImpl extends Denotation[ScriptImpl, PointedCube[Script]] {
    def denotation(impl: ScriptImpl) = PointedCube.Singleton(impl.script.!)
  }

  implicit object ParseImpl
      extends ContextualDenotation[(Map[Name, PointedCube[Value]], Env), Impl, PointedCube[Script]] {
    def defaultContext = (Map(), Env(Name("local")))

    def denotation(impl: Impl, localParamsEnv: (Map[Name, PointedCube[Value]], Env)) = impl match {
      case impl: FuncCallImpl => impl.!(localParamsEnv)
      case impl: ScriptImpl   => impl.!
    }
  }

  implicit object ParseFuncCall extends Denotation[FuncCall, PointedCubeCall] {
    def denotation(fc: FuncCall) = {
      val f = getFunc(fc.funcName.!)
      val args = fc.inputs.map { case (k, (_, v)) => k.! -> v.!! }
      f.reify(args)
    }
  }

  implicit object ParseDecoratorCall extends Denotation[DecoratorCall, PointedCubeCall] {
    def denotation(dc: DecoratorCall) = dc.call.!
  }

  implicit object ParseFunc extends Denotation[FuncDef, Func] {
    def denotation(fd: FuncDef) = {
      val FuncDef(name, params, input, inputName, impl) = fd
      val ps = params.map { case (k, (_, v)) => k.! -> v.!! }
      Func(name.!, ps, input.!, inputName.value, impl.!())
    }
  }

  implicit object ParsePackage extends Denotation[PackageDef, PointedCubePackage] {
    def denotation(pd: PackageDef) = {
      val PackageDef(decorators, name, inputs, output, impl) = pd
      val inputParams = inputs.map { case (k, (_, v)) => k.! -> v.!! }
      val outputParams = Assignments(Seq(output)).map { case (k, (_, v)) => k.! -> v.!! }
      val axes = (inputParams ++ outputParams).values.map(_.cases.vars).fold(Set())(_ union _)
      PointedCubePackage(
        name = name.!,
        cases = allCases.filterVars(axes),
        inputs = inputParams,
        outputs = outputParams.head,
        decorators = decorators.calls.map(_.!),
        rawScript = impl.!
      )
    }
  }

  implicit object ParseTask extends Denotation[TaskDef, PointedCubeTask] {
    def denotation(td: TaskDef) = {
      val TaskDef(decorators, name, env, inputs, outputs, impl) = td
      val taskEnv = env.!!
      val inputEnvs = inputs.map { case (k, (em, _)) => k.! -> em.!! }
      val outputEnvs = outputs.map { case (k, (em, _)) => k.! -> em.!! }
      val inputParams = inputs.map { case (k, (_, v)) => k.! -> v.!((Map(), taskEnv)) }
      val outputParams = outputs.map { case (k, (_, v)) => k.! -> v.!! }
      val localParams = inputParams ++ outputParams
      val script = impl.!((localParams, taskEnv))
      val calls = decorators.calls.map(_.!)
      val inputParamAxes = inputParams.values.map(_.cases.vars)
      val callParamAxes = calls.map(_.cases.vars)
      val axes = (callParamAxes ++ inputParamAxes).fold(Set())(_ union _)
      new PointedCubeTask(
        name.!,
        taskEnv,
        allCases.filterVars(axes),
        inputParams,
        inputEnvs,
        outputParams,
        outputEnvs,
        calls,
        script
      )
    }
  }

  implicit object ParseTaskRef1 extends Denotation[TaskRef1, PointedCube[Task]] {
    def denotation(tr: TaskRef1) = getTask(tr.name.!).select(tr.indices.!)
  }

  implicit object ParseTaskRefN extends Denotation[TaskRefN, Cube[PointedCube[Task]]] {
    def denotation(tr: TaskRefN) = getTask(tr.name.!).currySelectMany(tr.indices.!)
  }

  implicit object ParsePlan extends Denotation[PlanDef, Plan] {
    def denotation(pd: PlanDef) = new Plan(pd.taskRefs.map(_.!.map(_.default)))
  }

  implicit object ParseModule extends Denotation[ModuleDef, Module] {
    def denotation(md: ModuleDef) = {
      val ModuleDef(name, params, defs) = md
      val ps = params.map { case (k, (_, v)) => k.! -> v.!! }
      Module(name.!, ps, defs)
    }
  }

  /** From definitions, deduces all declared axes.
    *
    * @param stmts
    *   All definitions
    * @return
    *   All declared axes; fail if there is any axis mis-alignments.
    */
  def getAllCases(stmts: Iterable[Statement]): PointedCaseCube = {
    val axesOccurrences: Iterable[(Name, Iterable[String])] =
      stmts.view.flatMap(_.recursiveChildren).collect { case DictLiteral(axisName, assignments) =>
        (axisName.!, assignments.keys)
      }
    val axes = axesOccurrences
      .groupBy(_._1)
      .view
      .map { case (axis, xs) =>
        val keys = xs.map(_._2.toSeq).toSeq
        val keys0 = keys.head
        if (keys.forall(_ == keys0))
          axis -> PointedSet(keys0.toSet, keys0.head)
        else throw AxesAlignmentException(axis, keys0, keys.find(_ != keys0).get)
      }
      .toMap
    PointedCaseCube(axes)
  }

  def semanticParse(
      stmts: Iterable[Statement],
      prefix: String = "",
      extraArgs: Map[Name, PointedCube[Value]] = Map()
  ): Unit = {
    val all = getAllCases(stmts)
    allCases = allCases outerJoin all // updates all cases

    stmts foreach {
      case ValDef(id, value) =>
        val a = id.!
        if (valueTable contains a) throw DuplicateDefinitionException("Value", a.name)
        else valueTable += Name(prefix + a.name) -> value.!!
      case GlobalValDef(id, value) =>
        val a = id.!
        if (globalValueTable contains a) throw DuplicateDefinitionException("Global value", a.name)
        else globalValueTable += Name(prefix + a.name) -> value.!!
      case fd: FuncDef =>
        val f = fd.name.!
        if (funcTable contains f) throw DuplicateDefinitionException("Function", f.name)
        else {
          val func = fd.!
          funcTable += Name(prefix + f.name) -> func.withNewArgs(extraArgs)
        }
      case td: TaskDef =>
        val t = td.name.!
        if (taskTable contains t) throw DuplicateDefinitionException("Task", t.name)
        else {
          val task = td.!
          taskTable += Name(prefix + t.name) -> task.withNewArgs(extraArgs)
        }
      case pd: PackageDef =>
        val p = pd.name.!
        if (packageTable contains p) throw DuplicateDefinitionException("Package", p.name)
        else {
          val pack = pd.!
          packageTable += Name(prefix + p.name) -> pack.withNewArgs(extraArgs)
        }
      case pd: PlanDef =>
        val p = pd.name.!
        if (planTable contains p) throw DuplicateDefinitionException("Plan", p.name)
        else {
          val plan = pd.!
          planTable += Name(prefix + p.name) -> plan
        }
      case md: ModuleDef =>
        val m = md.name.!
        if (moduleTable contains m) throw DuplicateDefinitionException("Module", m.name)
        else {
          val module = md.!
          moduleTable += Name(prefix + m.name) -> module.withNewArgs(extraArgs)
        }
      case mi: ModuleInstantiation =>
        val m = moduleTable.getOrElse(mi.module.!, throw UndefinedException("Module", mi.module.!))
        val prefix = mi.name.! + "."
        val args = mi.params.map { case (k, (_, v)) => Name(prefix + k.!) -> v.!! }
        for ((k, v) <- args)
          valueTable += Name(prefix + k.name) -> v
        semanticParse(m.defs, prefix, args)
    }
  }

  /** Reads a Hypermake script while expanding all import statements. This function processes `import` statements.
    *
    * @param f
    *   Script file to be read
    * @return
    *   A sequence of top-level definitions
    */
  def readFileToStmts(f: File): Seq[Statement] =
    readLinesToStmts(f.lines)

  /** Reads a stream of Hypermake script lines and parses them to statements. */
  def readLinesToStmts(lines: Iterable[String]): Seq[Statement] = {
    val content = lines.mkString("\n")

    val stmts = SyntacticParser.syntacticParse(content)
    val expandedStmts = stmts.flatMap {
      case ImportStatement(fn) =>
        readFileToStmts(resolveFile(fn))
      case stmt => Seq(stmt)
    }
    expandedStmts
  }

  def semanticParse(file: File): Unit = {
    semanticParse(readFileToStmts(file))
  }

  def parseTask(tr: TaskRef1) = tr.!.default

  def parseTarget(tr: TaskRefN) =
    plans.get(tr.name.!).map(_.targets).getOrElse(Seq(tr.!.map(_.default)))

}
