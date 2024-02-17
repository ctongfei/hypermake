package hypermake.semantics

import better.files._
import cats.instances.all._
import cats.syntax.unorderedTraverse._
import cats.syntax.monad._
import hypermake.collection._
import hypermake.core._
import hypermake.exception._
import hypermake.syntax._
import hypermake.syntax.ast._
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
    override def defaultContext = Env("local")

    def denotation(env: EnvModifier, parent: Env) =
      env.optEnv.fold(parent)(e => Env(e.str))
  }

  implicit object ParseIdentifier extends Denotation[Identifier, String] {
    def denotation(id: Identifier) = id.name
  }

  implicit object ParseAxisName extends Denotation[AxisName, Axis] {
    override def denotation(a: AxisName) = Axis(a.name)
  }

  implicit object ParseIdentifierPath extends Denotation[IdentifierPath, Path] {
    def denotation(idp: IdentifierPath): Path = Path(idp.path.map(_.name).toList)
  }

  implicit object ParseOutputRef extends Denotation[OutputRef, String] {
    def denotation(or: OutputRef) = or.str
  }

//  implicit object ParseCase extends Denotation[AxisIndices, Case] {
//    def denotation(is: AxisIndices): Case = Case {
//      is.underlying.map { case (id, k) => id.! -> k.key }
//    }
//  }

  implicit object ParseCaseCube extends Denotation[AxisIndices, CaseTensor] {
    def denotation(is: AxisIndices): CaseTensor = CaseTensor {
      is.map {
        case (a, Keys(ks)) => Axis(a.!) -> ks
        case (a, Star())   => Axis(a.!) -> getAxis(Axis(a.!)) // *: all values in axis a
      }
    }
  }

  implicit object ParseScript extends Denotation[Verbatim, Script] {
    def denotation(verb: Verbatim): Script = Script(verb.text)
  }

  implicit object ParseValRef
      extends ContextualDenotation[(Map[String, PointedTensor[Value]], Env), ValRef, PointedTensor[Value]] {
    def defaultContext = (Map(), Env.local)

    def denotation(r: ValRef, localsEnv: (Map[String, PointedTensor[Value]], Env)): PointedTensor[Value] = {
      val name = r.name.!
      val indices = r.indices.!
      val output = r.output.map(_.!)
      val (locals, env) = localsEnv
      locals.getOrElse(
        name.toString,
        output match {
          case Some(o) => // a.b[x].o; output of a task
            root.tasks(name).reduceSelected(indices, c => Value.Multiple(c.map(_.outputs(o)), env))
          case None => // a.b[x] or a.b.o
            root.values
              .get(name)
              .map(_.reduceSelected(indices, c => Value.Multiple(c, env)))
              .orElse(
                root.tasks
                  .get(name.init)
                  .map(_.reduceSelected(indices, c => Value.Multiple(c.map(_.outputs(name.last)), env)))
              )
              .getOrElse(root.packages(name).output.map(_.on(env)))
        }
      )
    }
  }

  implicit object ParseStringLiteral extends Denotation[StringLiteral, PointedTensor[Value]] {
    def denotation(sl: StringLiteral) = {
      val env = sl.envModifier.!(null) // no env modifier means pure input
      val v = if (env eq null) Value.Pure(sl.value) else Value.Input(sl.value, env)
      PointedTensor.Singleton(v)
    }
  }

  implicit object ParseDictLiteral extends Denotation[DictLiteral, PointedTensor[Value]] {
    def denotation(dl: DictLiteral) = {
      val DictLiteral(axis, assignments) = dl
      PointedTensor.OfNestedMap(
        axis.!,
        assignments.mapValuesE(ParseExpr.denotation(_, (Map(), Env("")))).pointed(getAxis(axis.!).default)
      )
    }
  }
//
//  implicit object ParseTaskOutputRef1 extends Denotation[TaskOutputRef1, PointedCube[Value.Output]] {
//    def denotation(tor: TaskOutputRef1) = {
//      val TaskOutputRef1(TaskRef1(task, indices), output) = tor
//      getTask(task.!).select(indices.!).map(_.outputs(output.!))
//    }
//  }

//  implicit object ParseTaskOutputRefN extends Denotation[TaskOutputRef, PointedCube[Value.Multiple]] {
//    def denotation(tor: TaskOutputRefN) = {
//      val TaskOutputRefN(TaskRefN(taskName, indicesN), output) = tor
//      val task = getTask(taskName.!)
//      val indices = indicesN.!
//      task.curry(indices.vars) map { t =>
//        Value.Multiple(
//          cases = t.selectMany(indices).map(_.outputs(output.!)),
//          env = task.env
//        )
//      }
//    }
//  }

  implicit object ParseExpr
      extends ContextualDenotation[(Map[String, PointedTensor[Value]], Env), Expr, PointedTensor[Value]] {
    def defaultContext = (Map(), Env.local)

    def denotation(e: Expr, localsEnv: (Map[String, PointedTensor[Value]], Env)) = e match {
      case sl: StringLiteral => sl.!
      case dl: DictLiteral   => dl.!
      case vr: ValRef        => vr.!(localsEnv)
    }
  }

  implicit object ParseFuncCallImpl
      extends ContextualDenotation[(Map[String, PointedTensor[Value]], Env), FuncCallImpl, PointedTensor[Script]] {
    def defaultContext = (Map(), Env.local)

    def denotation(impl: FuncCallImpl, localParamsEnv: (Map[String, PointedTensor[Value]], Env)) = {
      val func = root.functions(impl.call.name.!)
      val funcArgs = impl.call.args
        .map { case (p, (_, a)) => p.! -> a.!(localParamsEnv) }
      func.withNewArgs(funcArgs).impl
    }
  }

  implicit object ParseScriptImpl extends Denotation[ScriptImpl, PointedTensor[Script]] {
    def denotation(impl: ScriptImpl) = PointedTensor.Singleton(impl.script.!)
  }

  implicit object ParseImpl
      extends ContextualDenotation[(Map[String, PointedTensor[Value]], Env), TaskImpl, PointedTensor[Script]] {
    def defaultContext = (Map(), Env("local"))

    def denotation(impl: TaskImpl, localParamsEnv: (Map[String, PointedTensor[Value]], Env)) = impl match {
      case impl: FuncCallImpl => impl.!(localParamsEnv)
      case impl: ScriptImpl   => impl.!
    }
  }

  implicit object ParseFuncCall extends Denotation[ast.Call, PointedTensor[Script]] {
    def denotation(fc: ast.Call) = {
      val f = root.functions(fc.name.!)
      val args = fc.args.map { case (k, (_, v)) => k.! -> v.!! }
      f.withNewArgs(args).impl
    }
  }

  implicit object ParseDecoratorCall extends Denotation[ast.Decoration, Decorator] {
    def denotation(dc: ast.Decoration) = {
      val obj = root.objects(dc.obj.!)
      Decorator.fromObj(obj)
    }
  }

  implicit object ParseFuncDef extends Denotation[FuncDef, Definition[PointedFuncTensor]] {
    def denotation(fd: FuncDef) = {
      val FuncDef(name, params, impl) = fd
      val ps = params.map { case (k, (_, v)) => k.! -> v.!! }
      val implTensor = impl.!()
      Definition(
        name.!,
        new PointedFuncTensor(name.!, implTensor.cases, ps.keySet, implTensor)
      )
    }
  }

  implicit object ParsePackageDef extends Denotation[PackageDef, Definition[PointedPackageTensor]] {
    def denotation(pd: PackageDef) = {
      val PackageDef(decorators, name, inputs, output, impl) = pd
      val inputParams = inputs.map { case (k, (_, v)) => k.! -> v.!! }
      val outputParams = Assignments(Seq(output)).map { case (k, (_, v)) => k.! -> v.!! }
      val axes = (inputParams ++ outputParams).values.map(_.cases.vars).fold(Set())(_ union _)
      val outputFileName = outputParams.head._1 -> outputParams.head._2.map(_.asIfPure)
      Definition(
        name.!,
        PointedPackageTensor(
          name = name.!,
          cases = allCases.filterVars(axes),
          inputs = inputParams,
          outputFileName = outputFileName,
          decorators = decorators.calls.map(_.!),
          rawScript = impl.!
        )
      )
    }
  }

  implicit object ParseTaskDef extends Denotation[TaskDef, Definition[PointedTaskTensor]] {
    def denotation(td: TaskDef) = {
      val TaskDef(decorators, name, env, inputs, outputs, impl) = td
      val taskEnv = env.!!
      val inputEnvs = inputs.map { case (k, (em, _)) => k.! -> em.!! }
      val outputEnvs = outputs.map { case (k, (em, _)) => k.! -> em.!! }
      val inputParams = inputs.map { case (k, (_, v)) => k.! -> v.!((Map(), taskEnv)) }
      val outputParams = outputs.map { case (k, (_, v)) => k.! -> v.!!.map(_.asIfPure) }
      val localParams = inputParams ++ outputParams
      val script = impl.!((localParams, taskEnv))
      val calls = decorators.calls.map(_.!)
      val inputParamAxes = inputParams.values.map(_.cases.vars)
      val callParamAxes = calls.map(_.script.cases.vars)
      val axes = (callParamAxes ++ inputParamAxes).fold(Set())(_ union _)
      Definition(
        name.!,
        new PointedTaskTensor(
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
      )
    }
  }

//  implicit object ParseTaskRef1 extends Denotation[TaskRef1, PointedCube[Task]] {
//    def denotation(tr: TaskRef1) = getTask(tr.name.!).select(tr.indices.!)
//  }

  implicit object ParseTaskRefN extends Denotation[TaskRef, Tensor[PointedTensor[Task]]] {
    def denotation(tr: TaskRef) = root.tasks(tr.name.!).currySelectMany(tr.indices.!)
  }

  implicit object ParseValDef extends Denotation[ValDef, Definition[PointedTensor[Value]]] {
    def denotation(vd: ValDef) = {
      val ValDef(id, value) = vd
      Definition(id.!, value.!!)
    }
  }

  implicit object ParsePlanDef extends Denotation[PlanDef, Definition[Plan]] {
    def denotation(pd: PlanDef) = Definition(
      pd.name.!,
      new Plan(pd.taskRefs.map(_.!.map(_.default)))
    )
  }

  implicit object ParseDef extends Denotation[Def, Definition[_]] {
    def denotation(d: Def) = d match {
      case vd: ValDef     => vd.!
      case fd: FuncDef    => fd.!
      case td: TaskDef    => td.!
      case pd: PackageDef => pd.!
      case pd: PlanDef    => pd.!
    }
  }

  /** From definitions, deduces all declared axes.
    *
    * @param stmts
    *   All definitions
    * @return
    *   All declared axes; fail if there is any axis mis-alignments.
    */
  def getAllCases(stmts: Iterable[Statement]): PointedCaseTensor = {
    val axesOccurrences: Iterable[(Axis, Iterable[String])] =
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
    PointedCaseTensor(axes)
  }

  def semanticParse(stmts: Iterable[Statement], topLevel: Boolean = false): Obj = {
    val all = getAllCases(stmts)
    allCases = allCases outerJoin all // updates all cases
    val root = if (topLevel) ctx.root else new Obj

    stmts foreach {
      case ImportFile(fileName, moduleName) =>
        val obj = semanticParseFile(resolveFile(fileName))
        moduleName match {
          case Some(name) => root.addDef(Definition(name.!, obj))
          case None       => root.merge(obj)
        }
      case ImportObject(modulePath, moduleName) =>
        val obj = semanticParseFile(resolveModule(modulePath.!.toString))
        moduleName match {
          case Some(name) => root.addDef(Definition(name.!, obj))
          case None       => root.merge(obj)
        }
      case d: Def =>
        root.addDef(d.!)
    }
    root
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

  /** Reads a stream of Hypermake script lines and parses them to statements.
    */
  def readLinesToStmts(lines: Iterable[String]): Seq[Statement] = {
    val content = lines
      .filterNot(_.trim == "")
      .mkString("\n")

    syntacticParse(content)
  }

  def semanticParseFile(file: File, topLevel: Boolean = false): Obj = {
    semanticParse(readFileToStmts(file), topLevel)
  }

  def parseTask(tr: TaskRef) = tr.!.allElements.head.default // TODO: make sure that there is only 1 in the cube

  def parseTarget(tr: TaskRef) =
    root.plans.get(tr.name.!).map(_.targets).getOrElse(Seq(tr.!.map(_.default)))

}
