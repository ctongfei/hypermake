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

  implicit class ContextualDenotationExtension[C, S, D](s: S)(implicit
      ssi: ContextualDenotation[C, S, D]
  ) {
    def !(ctx: C = ssi.defaultContext) = ssi.denotation(s, ctx)

    def !! : D = ssi.denotation(s, ssi.defaultContext)
  }

  implicit object ParseFileSys extends ContextualDenotation[FileSys, FileSysModifier, FileSys] {
    override def defaultContext = FileSys("local")

    def denotation(fsm: FileSysModifier, parent: FileSys) =
      fsm.optFs.fold(parent)(e => FileSys(e.str))
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

  implicit object ParseCaseCube extends Denotation[AxisIndices, Shape] {
    def denotation(is: AxisIndices): Shape = Shape {
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
      extends ContextualDenotation[
        (Map[String, PointedTensor[Value]], FileSys),
        ValRef,
        PointedTensor[
          Value
        ]
      ] {
    def defaultContext = (Map(), FileSys.local)

    def denotation(
        r: ValRef,
        localsFs: (Map[String, PointedTensor[Value]], FileSys)
    ): PointedTensor[Value] = {
      val name = r.name.!
      val indices = r.indices.!
      val output = r.output.map(_.!)
      val (locals, fs) = localsFs

      def selectPotentiallyMultipleValues[A](
          a: PointedTensor[A],
          c: Shape,
          f: A => Value
      ): PointedTensor[Value] = {
        val isSingleton = c.underlying.values.forall(_.size == 1)
        if (isSingleton) {
          val defaultCase = Case(c.underlying.map { case (a, ks) => a -> ks.head })
          a.select(defaultCase).map(f)
        } else a.reduceSelected(c, x => Value.Multiple(x.map(f), fs))
      }

      locals.getOrElse(
        name.toString,
        output match {
          case Some(o) => // a.b[x].o; output of a task
            selectPotentiallyMultipleValues[Task](root.tasks(name), indices, _.outputs(o))
          case None => // a.b[x] or a.b.o
            root.values
              .get(name)
              .map(selectPotentiallyMultipleValues[Value](_, indices, x => x))
              .orElse(
                root.tasks
                  .get(name.init)
                  .map(selectPotentiallyMultipleValues[Task](_, indices, _.outputs(name.last)))
              )
              .getOrElse(root.packages(name).output.map(_.on(fs)))
        }
      )
    }
  }

  implicit object ParseStringLiteral extends Denotation[StringLiteral, PointedTensor[Value]] {
    def denotation(sl: StringLiteral) = {
      val fs = sl.fsModifier.!(null) // no file sys modifier means pure input
      val v = if (fs eq null) Value.Pure(sl.value) else Value.Input(sl.value, fs)
      PointedTensor.Singleton(v)
    }
  }

  implicit object ParseDictLiteral extends Denotation[DictLiteral, PointedTensor[Value]] {
    def denotation(dl: DictLiteral) = {
      val DictLiteral(axis, assignments) = dl
      PointedTensor.OfNestedMap(
        axis.!,
        assignments
          .mapValuesE(ParseExpr.denotation(_, (Map(), FileSys(""))))
          .pointed(getAxis(axis.!).default)
      )
    }
  }

  implicit object ParseExpr
      extends ContextualDenotation[
        (Map[String, PointedTensor[Value]], FileSys),
        Expr,
        PointedTensor[
          Value
        ]
      ] {
    def defaultContext = (Map(), FileSys.local)

    def denotation(e: Expr, localsFs: (Map[String, PointedTensor[Value]], FileSys)) = e match {
      case sl: StringLiteral => sl.!
      case dl: DictLiteral   => dl.!
      case vr: ValRef        => vr.!(localsFs)
    }
  }

  implicit object ParseFuncCallImpl
      extends ContextualDenotation[
        (Map[String, PointedTensor[Value]], FileSys),
        FuncCallImpl,
        PointedTensor[Script]
      ] {
    def defaultContext = (Map(), FileSys.local)

    def denotation(
        impl: FuncCallImpl,
        localParamsFs: (Map[String, PointedTensor[Value]], FileSys)
    ) = {
      val func = root.functions(impl.call.name.!)
      val funcArgs = impl.call.args
        .map { case (p, (_, a)) => p.! -> a.!(localParamsFs) }
      func.withNewArgs(funcArgs).impl
    }
  }

  implicit object ParseScriptImpl extends Denotation[ScriptImpl, PointedTensor[Script]] {
    def denotation(impl: ScriptImpl) = PointedTensor.Singleton(impl.script.!)
  }

  implicit object ParseImpl
      extends ContextualDenotation[
        (Map[String, PointedTensor[Value]], FileSys),
        TaskImpl,
        PointedTensor[Script]
      ] {
    def defaultContext = (Map(), FileSys("local"))

    def denotation(impl: TaskImpl, localParamsFs: (Map[String, PointedTensor[Value]], FileSys)) =
      impl match {
        case impl: FuncCallImpl => impl.!(localParamsFs)
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
      val Decoration(clsName, optArgs) = dc
      optArgs match {
        case None =>
          val obj = root.objects(clsName.!)
          Decorator.fromObj(obj)
        case Some(args) =>
          val cls = root.classes(clsName.!)
          val obj = cls.instantiate(args.map { case (k, (_, v)) => k.! -> v.!! })
          Decorator.fromObj(obj)
      }
    }
  }

  implicit def ParseFuncDef: Denotation[FuncDef, Definition[PointedFuncTensor]] = {
    case FuncDef(name, params, impl) =>
      val ps = params.map { case (k, (_, v)) => k.! -> v.!! }
      val implTensor = impl.!()
      Definition(
        name.!,
        new PointedFuncTensor(name.!, implTensor.shape, ps.keySet, implTensor)
      )
  }

  implicit def ParsePackageDef: Denotation[PackageDef, Definition[PointedPackageTensor]] = {
    case PackageDef(decorators, name, inputs, output, impl) =>
      val inputParams = inputs.map { case (k, (_, v)) => k.! -> v.!! }
      val outputParams = Assignments(Seq(output)).map { case (k, (_, v)) => k.! -> v.!! }
      val axes = (inputParams ++ outputParams).values.map(_.shape.vars).fold(Set())(_ union _)
      val outputFileName = outputParams.head._1 -> outputParams.head._2.map(_.asIfPure)
      Definition(
        name.!,
        PointedPackageTensor(
          name = name.!,
          shape = allCases.filterVars(axes),
          inputs = inputParams,
          outputFileName = outputFileName,
          decorators = decorators.calls.map(_.!),
          rawScript = impl.!
        )
      )
  }

  implicit def ParseTaskDef: Denotation[TaskDef, Definition[PointedTaskTensor]] = {
    case TaskDef(decorators, name, fs, inputs, outputs, impl) =>
      val taskFs = fs.!!
      val inputFs = inputs.map { case (k, (em, _)) => k.! -> em.!! }
      val outputFs = outputs.map { case (k, (em, _)) => k.! -> em.!! }
      val inputParams = inputs.map { case (k, (_, v)) => k.! -> v.!((Map(), taskFs)) }
      val outputParams = outputs.map { case (k, (_, v)) => k.! -> v.!!.map(_.asIfPure) }
      val localParams = inputParams ++ outputParams
      val script = impl.!((localParams, taskFs))
      val calls = decorators.calls.map(_.!)
      val inputParamAxes = inputParams.values.map(_.shape.vars)
      val callParamAxes = calls.map(_.script.shape.vars)
      val axes = (callParamAxes ++ inputParamAxes).fold(Set())(_ union _)
      Definition(
        name.!,
        new PointedTaskTensor(
          name.!,
          taskFs,
          allCases.filterVars(axes),
          inputParams,
          inputFs,
          outputParams,
          outputFs,
          calls,
          script
        )
      )
  }

  implicit def ParseTaskRefN: Denotation[TaskRef, Tensor[PointedTensor[Task]]] = {
    case TaskRef(name, indices) =>
      root.tasks(name.!).currySelectMany(indices.!)
  }

  implicit def ParseValDef: Denotation[ValDef, Definition[PointedTensor[Value]]] = {
    case ValDef(id, value) =>
      Definition(id.!, value.!!)
  }

  implicit def ParseMembersImpl: Denotation[MembersImpl, Obj] = { impl =>
    Obj.fromDefs(impl.defs.map(_.!))
  }

  implicit def ParseInstantiationImpl: Denotation[InstantiationImpl, Obj] = { impl =>
    val cls = root.classes(impl.instantiation.name.!)
    cls.instantiate(impl.instantiation.args.map { case (k, (_, v)) => k.! -> v.!! })
  }

  implicit def ParseObjectDef: Denotation[ObjectDef, Definition[Obj]] = {
    case ObjectDef(name, impl) =>
      val obj = impl match {
        case impl: MembersImpl       => impl.!
        case impl: InstantiationImpl => impl.!
      }
      Definition[Obj](name.!, obj)
  }

  implicit def ParseClassDef: Denotation[ClassDef, Definition[Cls]] = {
    case ClassDef(name, inputs, impl) =>
      val cls = impl match {
        case impl: MembersImpl       => impl.!
        case impl: InstantiationImpl => impl.!
      }
      Definition[Cls](name.!, Cls(name.!, inputs.map { case (k, (_, v)) => k.! -> v.!! }, cls))
  }

  implicit def ParsePlanDef: Denotation[PlanDef, Definition[Plan]] = {
    case PlanDef(name, taskRefs) =>
      Definition[Plan](name.!, new Plan(taskRefs.map(_.!.map(_.default))))
  }

  implicit def ParseDef: Denotation[Def, Definition[_]] = {
    case vd: ValDef     => vd.!
    case fd: FuncDef    => fd.!
    case td: TaskDef    => td.!
    case pd: PackageDef => pd.!
    case pd: PlanDef    => pd.!
    case cd: ClassDef   => cd.!
    case od: ObjectDef  => od.!
  }

  /** From definitions, deduces all declared axes.
    *
    * @param stmts
    *   All definitions
    * @return
    *   All declared axes; fail if there is any axis mis-alignments.
    */
  def getAllCases(stmts: Iterable[Statement]): PointedShape = {
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
    PointedShape(axes)
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
          case None       => root.addDef(Definition(modulePath.!, obj))
        }
      case d: Def =>
        root.addDef(d.!)
    }
    root
  }

  /** Reads a Hypermake script while expanding all import statements. This function processes
    * `import` statements.
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

  def parseTask(tr: TaskRef) =
    tr.!.allElements.head.default // TODO: make sure that there is only 1 in the cube

  def parseTarget(tr: TaskRef) =
    root.plans.get(tr.name.!).map(_.targets).getOrElse(Seq(tr.!.map(_.default)))

}
