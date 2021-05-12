package hypermake.semantics

import better.files._
import fastparse._

import scala.collection._
import cats.syntax.unorderedTraverse._
import cats.instances.all._
import hypermake.core._
import hypermake.exception._
import hypermake.execution.RuntimeContext
import hypermake.syntax._
import hypermake.util._


/**
 * A semantic parsing run.
 * @param ctx Global context
 */
class SemanticParser(implicit val ctx: ParsingContext) {

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
    def denotation(is: IndicesN): CaseCube = CaseCube { is.map {
      case (a, Keys(ks)) => a.! -> ks
      case (a, Star()) => a.! -> getAxis(a.!) // *: all values in axis a
    }}
  }

  implicit object ParseScript extends Denotation[Verbatim, Script] {
    def denotation(verb: Verbatim): Script = Script(verb.text)
  }

  implicit object ParseValRef extends ContextualDenotation[Map[Name, PointedCube[Value]], ValRef, PointedCube[Value]] {
    def defaultContext = Map()
    def denotation(r: ValRef, locals: Map[Name, PointedCube[Value]]): PointedCube[Value] = {
      val name = r.name.!
      locals.getOrElse(name, getValue(name))  // local variables override global variables
    }
  }

  implicit object ParseStringLiteral extends Denotation[StringLiteral, PointedCube[Value]] {
    def denotation(sl: StringLiteral) = {
      val env = sl.envModifier.!(Env.Empty)  // no env modifier means pure input
      val v = if (env == Env.Empty) Value.Pure(sl.value) else Value.Input(sl.value, env)
      PointedCube.Singleton(v)
    }
  }

  implicit object ParseDictLiteral extends Denotation[DictLiteral, PointedCube[Value]] {
    def denotation(dl: DictLiteral) = {
      val DictLiteral(axis, assignments) = dl
      PointedCube.OfNestedMap(
        axis.!,
        assignments.mapValuesE(ParseExpr.denotation(_, Map())).pointed(getAxis(axis.!).default)
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
          cases = t.selectMany(indices).allElements.map(_.outputs(output.!)),
          env = task.env
        )
      }
    }
  }

  implicit object ParseExpr extends ContextualDenotation[Map[Name, PointedCube[Value]], Expr, PointedCube[Value]] {
    def defaultContext = Map()
    def denotation(e: Expr, locals: Map[Name, PointedCube[Value]]) = e match {
      case sl: StringLiteral => sl.!
      case dl: DictLiteral => dl.!
      case vr: ValRef => vr.!(locals)
      case tor: TaskOutputRef1 => tor.!
      case tor: TaskOutputRefN => tor.!
    }
  }

  implicit object ParseFuncCallImpl extends ContextualDenotation[Map[Name, PointedCube[Value]], FuncCallImpl, PointedCube[Script]] {
    def defaultContext = Map()
    def denotation(impl: FuncCallImpl, localParams: Map[Name, PointedCube[Value]]) = {
      val func = getFunc(impl.call.funcName.!)
      val funcArgs = impl.call.inputs
        .map { case (p, (_, a)) => p.! -> a.!(localParams) }.toMap
        .unorderedSequence
      funcArgs.map(func.reify)
    }
  }

  implicit object ParseScriptImpl extends Denotation[ScriptImpl, PointedCube[Script]] {
    def denotation(impl: ScriptImpl) = PointedCube.Singleton(impl.script.!)
  }

  implicit object ParseImpl extends ContextualDenotation[Map[Name, PointedCube[Value]], Impl, PointedCube[Script]] {
    def defaultContext = Map()
    def denotation(impl: Impl, localParams: Map[Name, PointedCube[Value]]) = impl match {
      case impl: FuncCallImpl => impl.!(localParams)
      case impl: ScriptImpl => impl.!
    }
  }

  implicit object ParseFunc extends Denotation[FuncDef, Func] {
    def denotation(fd: FuncDef) = {
      val FuncDef(name, inputs, outputs, impl) = fd
      Func(name.!, (inputs ++ outputs).map(_.!), impl.script.!)
    }
  }

  implicit object ParsePackage extends Denotation[PackageDef, Package] {
    def denotation(pd: PackageDef) = {
      val PackageDef(decorators, name, inputs, impl) = pd
      val inputParams = inputs.map { case (k, (_, v)) => k.! -> v.!! }
      val axes = inputParams.values.map(_.cases.vars).fold(Set())(_ union _)
      Package(name.!, allCases.filterVars(axes), inputParams, impl.!(inputParams))
    }
  }

  implicit object ParseTask extends Denotation[TaskDef, PointedCubeTask] {
    def denotation(td: TaskDef) = {
      val TaskDef(decorators, name, env, inputs, outputs, impl) = td
      val taskEnv = env.!!
      val inputEnvs = inputs.map { case (k, (em, _)) => k.! -> em.!! }
      val outputEnvs = outputs.map { case (k, (em, _)) => k.! -> em.!! }
      val inputParams = inputs.map { case (k, (_, v)) => k.! -> v.!! }
      val outputParams = outputs.map { case (k, (_, v)) => k.! -> v.!! }
      val localParams = inputParams ++ outputParams
      val axes = inputParams.values.map(_.cases.vars).fold(Set())(_ union _)
      val script = impl.!(localParams)
      new PointedCubeTask(
        name.!, taskEnv, allCases.filterVars(axes),
        inputParams, inputEnvs, outputParams, outputEnvs,
        script
      )
    }
  }

  implicit object ParseTaskRefN extends Denotation[TaskRefN, Cube[Task]] {
    def denotation(tr: TaskRefN) = getTask(tr.name.!).selectMany(tr.indices.!)
  }

  implicit object ParsePlan extends Denotation[PlanDef, Plan] {
    def denotation(pd: PlanDef) = new Plan(pd.taskRefs.map(_.!))
  }

  /**
   * From definitions, deduces all declared axes.
   * @param stmts All definitions
   * @return All declared axes; fail if there is any axis mis-alignments.
   */
  def getAllCases(stmts: Iterable[Statement]): PointedCaseCube = {
    val axesOccurrences: Iterable[(Name, Iterable[String])] =
      stmts.view.flatMap(_.recursiveChildren).collect {
        case DictLiteral(axisName, assignments) => (axisName.!, assignments.keys)
      }
    val axes = axesOccurrences.groupBy(_._1).view.map { case (axis, xs) =>
      val keys = xs.map(_._2.toSeq).toSeq
      val keys0 = keys.head
      if (keys.forall(_ == keys0))
        axis -> PointedSet(keys0.toSet, keys0.head)
      else throw AxesAlignmentException(axis, keys0, keys.find(_ != keys0).get)
    }.toMap
    PointedCaseCube(axes)
  }

  def semanticParse(stmts: Iterable[Statement]): Unit = {
    val all = getAllCases(stmts)
    allCases = allCases outerJoin all  // updates all cases

    stmts foreach {
      case ValDef(id, value) =>
        val a = id.!
        if (valueTable contains a) throw DuplicatedDefinitionException("Value", a.name)
        else {
          valueTable += a -> value.!!
        }
      case fd: FuncDef => // TODO: decorator not handled
        val f = fd.name.!
        if (funcTable contains f) throw DuplicatedDefinitionException("Function", f.name)
        else {
          val func = fd.!
          funcTable += f -> func
        }
      case td: TaskDef => // TODO: decorator not handled
        val t = td.name.!
        if (taskTable contains t) throw DuplicatedDefinitionException("Task", t.name)
        else {
          val task = td.!
          taskTable += t -> task
        }
      case pd: PackageDef => // TODO: decorator not handled
        val p = pd.name.!
        if (packageTable contains p) throw DuplicatedDefinitionException("Package", p.name)
        else {
          val pack = pd.!
          packageTable += p -> pack
        }
      case pd: PlanDef =>
        val p = pd.name.!
        if (planTable contains p) throw DuplicatedDefinitionException("Plan", p.name)
        else {
          val plan = pd.!
          planTable += p -> plan
        }
    }
  }

  /**
   * Reads a Forge script while expanding all import statements. This function processes `import` statements.
   * @param f Script file to be read
   * @param ctx Context
   * @return A sequence of top-level definitions
   */
  def readFileToStmts(f: File): Seq[Statement] = {
    val content = f.lines.mkString("\n")
    val stmts = SyntacticParser.syntacticParse(content)
    val expandedStmts = stmts.flatMap {
      case ImportStatement(fn, importIndices) =>
        readFileToStmts(resolveFile(fn))
      case stmt => Seq(stmt)
    }
    expandedStmts
  }

  def semanticParse(file: File): Unit = {
    semanticParse(readFileToStmts(file))
  }

  def parseTarget(tr: TaskRefN) = {
    planTable.get(tr.name.!).map(_.targets).getOrElse(Seq(tr.!))
  }

}
