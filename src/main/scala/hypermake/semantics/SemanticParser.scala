package hypermake.semantics

import scala.collection._

import better.files._

import hypermake.collection._
import hypermake.core._
import hypermake.exception._
import hypermake.syntax._
import hypermake.syntax.ast._
import hypermake.util._

/**
 * A semantic parsing run in a specific module/scope.
 * @param ctx Global context
 */
class SemanticParser(val scope: Obj)(implicit val ctx: Context) {

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
    def denotation(or: OutputRef) = or.name.name
  }

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
        (PointedArgsTensor[Value], FileSys),
        ValRef,
        PointedTensor[Value]
      ] {
    def defaultContext = (PointedArgsTensor(Map()), FileSys.local)

    def denotation(
        vr: ValRef,
        localsFs: (PointedArgsTensor[Value], FileSys)
    ): PointedTensor[Value] = {
      val name = vr.name.!
      val indices = vr.indices.!
      val output = vr.output.map(_.!)
      val (locals, fs) = localsFs

      def selectPotentiallyMultipleValues[A](
          a: PointedTensor[A],
          s: Shape,
          f: A => Value
      ): PointedTensor[Value] = {
        val isSingleton = s.underlying.values.forall(_.size == 1)
        if (isSingleton) {
          val defaultCase = Case(s.underlying.map { case (a, ks) => a -> ks.head })
          a.select(defaultCase).map(f)
        } else {
          a.reduceSelected(
            s,
            x => Value.Multiple(x.map(f), fs)
          )
        }
      }
      // TODO: refactor into some `resolve` functions
      val resolved = output match {
        case Some(o) => // a.b[x].o; output of a task
          for (task <- (scope.tasks ++ root.tasks).get(name))
            yield selectPotentiallyMultipleValues[Task](task, indices, _.outputs(o))
        case None => // a.b[x] or a.b.o
          {
            for (value <- locals.args.get(name.toString))
              yield selectPotentiallyMultipleValues[Value](value, indices, x => x)
          } // local variables
            .orElse { // variables in scope or global
              for (value <- (scope.values ++ root.values).get(name))
                yield selectPotentiallyMultipleValues[Value](value, indices, x => x)
            }
            .orElse { // non-indexed task outputs in scope or global
              val task = (scope.tasks ++ root.tasks).get(name.init)
              for (task <- task)
                yield selectPotentiallyMultipleValues[Task](task, indices, _.outputs(name.last))
            }
            .orElse { // package outputs in scope or global
              for (pack <- (scope.packages ++ root.packages).get(name))
                yield pack.output.map(_.on(fs))
            }
            .orElse { // output of service.setup
              for (serviceObj <- (scope.objects ++ root.objects).get(name))
                yield {
                  val service = serviceObj.asService
                  selectPotentiallyMultipleValues[Service](service, indices, _.setup.outputs.head._2)
                }
            }
      }
      resolved.getOrElse(throw ReferenceResolutionException(vr))
    }
  }

  implicit def ParseStringLiteral: Denotation[StringLiteral, PointedTensor[Value]] = { case (sl: StringLiteral) =>
    val fs = sl.fsModifier.!(null) // no file sys modifier means pure input
    val v = if (fs eq null) Value.Pure(sl.value) else Value.Input(sl.value, fs)
    PointedTensor.Singleton(v)
  }

  implicit def ParseDictLiteral: Denotation[DictLiteral, PointedTensor[Value]] = { case DictLiteral(axis, assignments) =>
    PointedTensor.OfNestedMap(
      axis.!,
      assignments
        .mapValuesE(ParseExpr.denotation(_, (PointedArgsTensor(Map()), FileSys(""))))
        .pointed(getAxis(axis.!).default)
    )
  }

  implicit object ParseExpr
      extends ContextualDenotation[
        (PointedArgsTensor[Value], FileSys),
        Expr,
        PointedTensor[Value]
      ] {
    def defaultContext = (PointedArgsTensor(Map()), FileSys.local)

    def denotation(e: Expr, localsFs: (PointedArgsTensor[Value], FileSys)) = e match {
      case sl: StringLiteral => sl.!
      case dl: DictLiteral   => dl.!
      case vr: ValRef        => vr.!(localsFs)
    }
  }

  implicit def ParseAssignments: Denotation[Assignments, PointedArgsTensor[Value]] = { as =>
    PointedArgsTensor(as.map { case (k, (_, v)) => k.! -> v.!! })
  }

  implicit object ParseFuncCallImpl
      extends ContextualDenotation[
        (PointedArgsTensor[Value], FileSys),
        FuncCallImpl,
        Func
      ] {
    def defaultContext = (PointedArgsTensor(Map()), FileSys.local)

    def denotation(
        impl: FuncCallImpl,
        localParamsFs: (PointedArgsTensor[Value], FileSys)
    ) = {
      val func = root.functions(impl.call.name.!)
      val funcArgs = PointedArgsTensor(
        impl.call.args
          .map { case (p, (_, a)) => p.! -> a.!(localParamsFs) }
      )
      func.partial(funcArgs)
    }
  }

  implicit object ParseScriptImpl extends Denotation[ScriptImpl, PointedTensor[Script]] {
    def denotation(impl: ScriptImpl) = PointedTensor.Singleton(impl.script.!)
  }

  implicit object ParseImpl
      extends ContextualDenotation[
        (PointedArgsTensor[Value], FileSys),
        TaskImpl,
        Func
      ] {
    def defaultContext = (PointedArgsTensor(Map()), FileSys("local"))

    def denotation(impl: TaskImpl, localParamsFs: (PointedArgsTensor[Value], FileSys)) =
      impl match {
        case impl: FuncCallImpl => impl.!(localParamsFs)
        case impl: ScriptImpl   => Func("$anon", Params(Map()), Params(Map()), impl.!)
      }
  }

  implicit def ParseFuncCall: Denotation[ast.Call, Func] = { (fc: ast.Call) =>
    val f = root.functions(fc.name.!)
    val args = fc.args.!
    f.partial(args)
  }

  implicit def ParseDecoratorCall: Denotation[ast.Decoration, PointedDecoratorTensor] = { case Decoration(clsName, optArgs) =>
    optArgs match {
      case None =>
        val obj = root.objects(clsName.!)
        PointedDecoratorTensor.fromObj(obj)
      case Some(args) =>
        val cls = root.classes(clsName.!)
        val obj = cls.instantiate(args.!, "<anon-obj>")
        PointedDecoratorTensor.fromObj(obj)
    }
  }

  implicit def ParseFuncDef: Denotation[FuncDef, Definition[Func]] = { case fd @ FuncDef(name, params, outputs, impl) =>
    val ps = Params.fromArgs(params.!)
    val os = Params.fromArgs(outputs.!)
    val implTensor = impl.!().impl
    name.! := new Func((scope.prefix / name.!).toString, ps, os, implTensor)
  }

  implicit def ParsePackageDef: Denotation[PackageDef, Definition[PointedPackageTensor]] = { case PackageDef(decorators, name, inputs, output, impl) =>
    val inputArgs = inputs.!
    val definedOutputArgs = Assignments(output.toList).!
    val localArgs = inputArgs ++ definedOutputArgs
    val axes =
      (inputArgs ++ definedOutputArgs).args.values
        .map(_.shape.vars)
        .fold(Set())(_ union _)
    val script = impl.!((localArgs, FileSys("")))
    val scriptOutputParams = script.outputs
    val outputParams = {
      // explicitly defined single output
      if (definedOutputArgs.args.size == 1 && scriptOutputParams.params.isEmpty) {
        definedOutputArgs.args.head._1 -> definedOutputArgs.args.head._2.map(_.asIfPure)
        // no output defined, but inherited from calling function
      } else if (definedOutputArgs.args.isEmpty && scriptOutputParams.params.size == 1)
        scriptOutputParams.params.head match {
          case (name, None)    => name -> PointedTensor.Singleton(Value.Pure(name))
          case (name, Some(v)) => name -> v.map(_.asIfPure)
        }
      else throw PackageOutputException(name.name)
    }

    name.! :=
      PointedPackageTensor(
        name = (scope.prefix / name.!).toString,
        shape = allCases.filterVars(axes),
        inputs = inputArgs,
        outputFileName = outputParams,
        decorators = decorators.calls.map(_.!),
        rawScript = script.impl
      )
  }

  implicit def ParseTaskDef: Denotation[TaskDef, Definition[PointedTaskTensor]] = { case TaskDef(decorators, ephemeral, name, fs, inputs, outputs, impl) =>
    val taskFs = fs.!!
    val inputFs = inputs.map { case (k, (em, _)) => k.! -> em.!! }
    val outputFs = outputs.map { case (k, (em, _)) => k.! -> em.!! }
    val inputArgs = PointedArgsTensor(inputs.map { case (k, (_, v)) => k.! -> v.!((PointedArgsTensor(Map()), taskFs)) })
    val outputArgs = PointedArgsTensor(outputs.map { case (k, (_, v)) => k.! -> v.!!.map(_.asIfPure) })
    val localArgs = inputArgs ++ outputArgs
    val script = impl.!((localArgs, taskFs))
    val calls = decorators.calls.map(_.!)
    val inputArgsAxes = inputArgs.args.values.map(_.shape.vars)
    val decoratorAxes = calls.map(_.script.shape.vars)
    val axes = (decoratorAxes ++ inputArgsAxes).fold(Set())(_ union _)
    name.! :=
      new PointedTaskTensor(
        (scope.prefix / name.!).toString,
        taskFs,
        allCases.filterVars(axes),
        inputArgs,
        inputFs,
        outputArgs,
        outputFs,
        calls,
        script.impl,
        ephemeral
      )
  }

  implicit def ParseTaskRefN: Denotation[TaskRef, Tensor[PointedTensor[Task]]] = { case TaskRef(name, indices) =>
    root.tasks(name.!).currySelectMany(indices.!)
  }

  implicit def ParseValDef: Denotation[ValDef, Definition[PointedTensor[Value]]] = { case ValDef(id, value) =>
    id.! := value.!!
  }

  implicit object ParseMembersImpl extends ContextualDenotation[String, MembersImpl, Obj] {

    def defaultContext = ""

    def denotation(impl: MembersImpl, name: String) = {
      val obj = new Obj(scope.prefix / name)
      val objParser = new SemanticParser(obj)
      for (d <- impl.defs)
        obj.addDef(objParser.ParseDef.denotation(d))
      obj
    }
  }

  implicit def ParseInstantiationImpl: Denotation[InstantiationImpl, Cls] = { impl =>
    val cls = root.classes(impl.instantiation.name.!)
    cls.partial(impl.instantiation.args.!)
  }

  implicit def ParseObjectDef: Denotation[ObjectDef, Definition[Obj]] = { case ObjectDef(name, impl) =>
    val obj = impl match {
      case impl: MembersImpl       => impl.!(name.name)
      case impl: InstantiationImpl => impl.!.instantiate(PointedArgsTensor(Map()), name.name)
    }
    name.! := obj
  }

  implicit def ParseClassDef: Denotation[ClassDef, Definition[Cls]] = { case ClassDef(name, inputs, impl) =>
    val cls = impl match {
      case impl: MembersImpl       => impl.!(name.name)
      case impl: InstantiationImpl => impl.!.instantiate(PointedArgsTensor(Map()), name.name)
    }
    name.! := Cls(name.!, Params.fromArgs(inputs.!), cls)
  }

  implicit def ParsePlanDef: Denotation[PlanDef, Definition[Plan]] = { case PlanDef(name, taskRefs) =>
    name.! := Plan(taskRefs.map(_.!.map(_.default)))
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

  /**
   * From definitions, deduces all declared axes.
   *
   * @param stmts All definitions
   * @return All declared axes; fail if there is any axis mis-alignments.
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

  def semanticParse(stmts: Iterable[Statement], scope: Obj = ctx.root): Obj = {
    val all = getAllCases(stmts)
    allCases = allCases outerJoin all // updates all cases

    stmts foreach {
      case ImportFile(fileName, moduleName) =>
        val obj = semanticParseFile(resolveFile(fileName), scope)
        moduleName match {
          case Some(name) => scope.addDef(name.! := obj)
          case None       => scope.merge(obj)
        }
      case ImportObject(modulePath, moduleName) =>
        val newModuleName = moduleName match {
          case Some(name) => name.!
          case None       => modulePath.!.toString
        }
        val newObj = new Obj(scope.prefix / newModuleName)
        val obj = semanticParseFile(resolveModule(modulePath.!.toString), newObj)
        scope.addDef(newModuleName := obj)
      case d: Def =>
        scope.addDef(d.!)
    }
    scope
  }

  def addDefs(defs: Iterable[Definition[_]]): Unit =
    defs foreach scope.addDef

  // TODO: unify Local with other FileSys
  def addFallbackLocalFsDefs(): Unit = {
    // Add some default file systems
    val local =
      if (!root.objects.contains(Path("local"))) {
        val loc = Obj.fromDefs(
          Path("local"),
          Seq()
        )
        root.addDef("local" := loc)
        loc
      } else root.objects(Path("local"))

    if (!local.values.contains(Path("root")))
      local.addDef(
        "root" := PointedTensor.Singleton(Value.Pure("out"))
      )
  }

  /**
   * Reads a Hypermake script while expanding all import statements. This function processes `import` statements.
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
    val content = lines
      .filterNot(_.trim == "")
      .mkString("\n")

    syntacticParse(content)
  }

  def semanticParseFile(file: File, scope: Obj = ctx.root): Obj = {
    import hypermake.util.printing._
    try {
      semanticParse(readFileToStmts(file), scope)
    } catch {
      case e: java.nio.file.NoSuchFileException =>
        System.err.println(s"Workflow file ${K(file.pathAsString)} does not exist.")
        System.exit(1)
        throw e
    }
  }

  def parseTask(tr: TaskRef) = {
    try {
      Some(tr.!.allElements.head.default)
    } catch {
      case e: Exception => None
    }
  } // TODO: make sure that there is only 1 in the tensor

  def parseTarget(tr: TaskRef) =
    root.plans.get(tr.name.!).map(_.targets).getOrElse(Seq(tr.!.map(_.default)))

}
