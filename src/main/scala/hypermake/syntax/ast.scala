package hypermake.syntax

import hypermake.util._

import scala.collection._

object ast {

  /** Supertype of all node types in the Forge AST.
    */
  sealed trait Node {
    self =>

    /** A canonical string representation of this AST node, without any syntactic sugar. This string
      * should be parsed back to an identical AST object, and serves as the `toString()`
      * implementation.
      */
    def str: String

    /** Iterates over all of its immediate child nodes.
      */
    def children: Iterable[Node]

    /** Iterates over all of its recursive child nodes.
      */
    def recursiveChildren: Iterable[Node] = new Iterable[Node] {
      def iterator: Iterator[Node] = new Iterator[Node] { // DFS
        private[this] val stack = mutable.Stack(self)

        def hasNext = stack.nonEmpty

        def next() = {
          val curr = stack.pop()
          curr.children.toArray.reverseIterator foreach stack.push
          curr
        }
      }
    }

    override def toString = str
  }

  /** Represents any (potentially parameterized) String value. An [[Expr]] can be either a
    * [[Literal]] or a [[ValRef]].
    */
  sealed trait Expr extends Node

  case class Identifier(name: String) extends Node {
    def str = name

    def children = Nil
  }

  case class AxisName(name: String) extends Node {
    def str = name

    override def children = Nil
  }

  object AxisName {
    implicit object ordering extends Ordering[AxisName] {
      def compare(x: AxisName, y: AxisName) = x.name compare y.name
    }
  }

  case class Identifiers(ids: Seq[Identifier]) extends Node with SetWrapper[Identifier] {
    def str = ids.mkString(", ")

    def children = ids

    val underlying = ids.toSet
  }

  case class IdentifierPath(path: Seq[Identifier]) extends Node {
    val str: String = path.mkString(".")

    def children = path

    override def hashCode(): Int = str.hashCode
  }

  case class FileSysModifier(optFs: Option[IdentifierPath]) extends Node {
    def str = optFs.fold("")("@" + _.str)

    def children = optFs.toList
  }

  case class InlineCommand(command: String) extends Node {
    def str = command

    def children = Nil

    /** Executes this inline command while being parsed.
      */
    def result() = {
      import scala.sys.process._
      command.!!.split(" ") // TODO: conform to IFS?
    }
  }

  case class Verbatim(text: String) extends Node {
    def str = text.split('\n').map(l => "  " + l.trim).mkString("\n")

    def children = Nil
  }

  /** Represents a literal. A literal can be either a String singleton ([[StringLiteral]]) or a
    * nested dict literal ([[DictLiteral]]).
    */
  sealed trait Literal extends Expr

  case class StringLiteral(value: String, fsModifier: FileSysModifier = FileSysModifier(None))
      extends Literal {
    def str = s"${Escaper.Shell.escape(value)}$fsModifier"

    def children = fsModifier :: Nil
  }

  case class DictLiteral(axis: AxisName, assignments: Map[String, Expr]) extends Literal {
    def str = s"{$axis: ${assignments.map { case (k, v) => s"$k=$v" }.mkString(" ")}}"

    def children = Iterable(axis) ++ assignments.values
  }

  sealed trait KeyN extends Node

  case class Keys(keys: Set[String]) extends KeyN {
    def str = keys.mkString(" ")

    def children = Nil
  }

  case class Star() extends KeyN {
    def str = "*"

    def children = Nil
  }

  case class AxisIndex(axis: Identifier, keys: KeyN) extends Node {
    def str = s"$axis: $keys"

    def children = axis :: keys :: Nil
  }

  case class AxisIndices(indices: Seq[AxisIndex]) extends MapWrapper[Identifier, KeyN] with Node {
    def str = if (indices.isEmpty) "" else s"[${indices.mkString(", ")}]"

    def children = indices

    def underlying = orderedMap(indices.map { case AxisIndex(axis, keys) => (axis, keys) })
  }

  case class TaskRef(name: IdentifierPath, indices: AxisIndices) extends Node {
    def str = s"$name$indices"

    def children = Iterable(name) ++ indices.flatMap { case (k, v) => Iterable(k, v) }
  }

  case class OutputRef(name: Identifier) extends Node {
    def str = s".$name"
    def children = Nil
  }

  case class ValRef(name: IdentifierPath, indices: AxisIndices, output: Option[OutputRef])
      extends Expr {
    def str = s"$$$name$indices${output.fold("")(_.str)}"

    def children = Iterable(name) ++ indices.flatMap { case (k, v) => Iterable(k, v) }
  }

  case class Parameter(name: Identifier, fsModifier: FileSysModifier) extends Node {
    def str = s"$name$fsModifier"

    override def children = name :: fsModifier :: Nil
  }

  sealed trait Assignment extends Node

  case class ExplicitAssignment(param: Parameter, value: Expr) extends Assignment {
    def str = s"$param = $value"

    def children = Iterable(param, value)
  }

  case class RefAssignment(param: Parameter) extends Assignment { // param = $
    def str = s"$param = $$${param.name}"

    def children = Iterable(param)
  }

  case class SameNameAssignment(param: Parameter) extends Assignment { // out [= "out"]
    def str = s"""$param = "${param.name}""""

    def children = Iterable(param)
  }

  case class Assignments(assignments: Seq[Assignment])
      extends MapWrapper[Identifier, (FileSysModifier, Expr)]
      with Node {
    def str = assignments.mkString(", ")

    def children = assignments

    val underlying = orderedMap(assignments.map {
      case ExplicitAssignment(param, v) => (param.name, (param.fsModifier, v))
      case RefAssignment(param) =>
        (
          param.name,
          (param.fsModifier, ValRef(IdentifierPath(Seq(param.name)), AxisIndices(Nil), None))
        )
      case SameNameAssignment(param) =>
        (param.name, (param.fsModifier, StringLiteral(param.name.name, param.fsModifier)))
    })
  }

  case class Call(name: IdentifierPath, args: Assignments) extends Node {
    def str = s"$name($args)"

    def children = Iterable(name, args)
  }

  case class Decoration(cls: IdentifierPath, args: Option[Assignments]) extends Node {
    def str = args.fold(s"@$cls\n") { a => s"@$cls($a)\n" }

    def children = Iterable(cls) ++ args
  }

  case class DecoratorCalls(calls: Seq[Decoration]) extends Node {
    def str = calls.reverse.mkString("")

    def children = calls
  }

  sealed trait TaskImpl extends Node

  case class ScriptImpl(script: Verbatim) extends TaskImpl {
    def str = s":\n$script"

    def children = Iterable(script)
  }

  case class FuncCallImpl(call: Call) extends TaskImpl {
    def str = s" = $call"

    def children = Iterable(call)
  }

  sealed trait ObjectImpl extends Node

  case class MembersImpl(defs: Seq[Def]) extends ObjectImpl {
    def str = ":\n" + defs.map(d => s"  $d").mkString("\n")

    def children = defs
  }

  case class InstantiationImpl(instantiation: Call) extends ObjectImpl {
    def str = s" = $instantiation"

    def children = Iterable(instantiation)
  }

  sealed trait Statement extends Node

  sealed trait Def extends Statement

  case class ValDef(name: IdentifierPath, value: Expr) extends Def {
    def str = s"$name = $value"

    def children = Iterable(name, value)
  }

  case class TaskDef(
      decorators: DecoratorCalls,
      name: Identifier,
      fsModifier: FileSysModifier,
      inputs: Assignments,
      outputs: Assignments,
      impl: TaskImpl
  ) extends Def {
    def str = s"${decorators}task $name$fsModifier($inputs) -> ($outputs)$impl"

    def children = decorators.calls ++ Iterable(fsModifier, name, inputs, outputs, impl)
  }

  case class ServiceDef(
      decorators: DecoratorCalls,
      name: Identifier,
      fsModifier: FileSysModifier,
      inputs: Assignments,
      impl: TaskImpl
  ) extends Def {
    def str = s"${decorators}service $name($inputs)$impl"

    def children = decorators.calls ++ Iterable(name, inputs, impl)
  }

  case class PackageDef(
      decorators: DecoratorCalls,
      name: Identifier,
      inputs: Assignments,
      output: ExplicitAssignment,
      impl: ScriptImpl
  ) extends Def {
    def str = s"${decorators}package $name($inputs) -> $output$impl"

    def children = decorators.calls ++ Iterable(name, inputs, output, impl)
  }

  case class FuncDef(name: Identifier, params: Assignments, impl: TaskImpl) extends Def {
    def str = s"""def $name($params)$impl"""

    def children = Iterable(name, params, impl)
  }

  case class PlanDef(name: Identifier, taskRefs: Seq[TaskRef]) extends Def {
    def str = s"plan $name = {${taskRefs.mkString(" ")}}"

    def children = Iterable(name) ++ taskRefs
  }

  case class ClassDef(name: Identifier, params: Assignments, impl: ObjectImpl) extends Def {
    def str = s"class $name($params)$impl"

    def children = Iterable(name, params, impl)
  }

  case class ObjectDef(name: Identifier, impl: ObjectImpl) extends Def {
    def str = s"object $name$impl"

    def children = Iterable(name, impl)
  }

  case class ImportFile(fileName: String, moduleName: Option[Identifier]) extends Statement {
    def str = moduleName match {
      case Some(m) => s"import ${Escaper.Shell.escape(fileName)} as $m"
      case None    => s"import ${Escaper.Shell.escape(fileName)}"
    }

    def children = moduleName.toList
  }

  case class ImportObject(modulePath: IdentifierPath, moduleName: Option[Identifier])
      extends Statement {
    def str = moduleName match {
      case Some(m) => s"import $modulePath as $m"
      case None    => s"import $modulePath as $modulePath"
    }

    def children = modulePath.children ++ moduleName.toList
  }

}
