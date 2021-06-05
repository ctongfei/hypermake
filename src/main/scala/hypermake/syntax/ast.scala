package hypermake.syntax

import hypermake.collection.Name
import hypermake.util._

import scala.collection._

/**
 * Supertype of all node types in the Forge AST.
 */
sealed trait Node { self =>

  /**
   * A canonical string representation of this AST node, without any syntactic sugar.
   * This string should be parsed back to an identical AST object, and serves as the `toString()` implementation.
   */
  def str: String

  /**
   * Iterates over all of its immediate child nodes.
   */
  def children: Iterable[Node]

  /**
   * Iterates over all of its recursive child nodes.
   */
  def recursiveChildren: Iterable[Node] = new Iterable[Node] {
    def iterator: Iterator[Node] = new Iterator[Node] {  // DFS
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

/**
 * Represents any (potentially parameterized) String value.
 * An [[Expr]] can be either a [[Literal]] or a [[ValRef]], or a [[TaskOutputRef1]] or a [[TaskOutputRefN]].
 */
sealed trait Expr extends Node

case class Identifier(name: String) extends Node {
  def str = name
  def children = Nil
}

object Identifier {
  implicit object ordering extends Ordering[Identifier] {
    def compare(x: Identifier, y: Identifier) = x.name compare y.name
  }
}

case class Identifiers(ids: Seq[Identifier]) extends Node with SetWrapper[Identifier] {
  def str = ids.mkString(", ")
  def children = ids
  val underlying = ids.toSet
}

case class EnvModifier(optEnv: Option[Identifier]) extends Node {
  def str = optEnv.fold("")(_.name)
  def children = optEnv.toList
}

case class InlineCommand(command: String) extends Node {
  def str = command
  def children = Nil

  /**
   * Executes this inline command while being parsed.
   */
  def result() = {
    import scala.sys.process._
    command.!!.split(" ")  // TODO: conform to IFS?
  }
}

case class Verbatim(text: String) extends Node {
  def str = text.split('\n').map(l => "  " + l.trim).mkString("\n")
  def children = Nil
}

/**
 * Represents a literal. A literal can be either a String singleton ([[StringLiteral]])
 * or a nested dict literal ([[DictLiteral]]).
 */
sealed trait Literal extends Expr

case class StringLiteral(value: String, envModifier: EnvModifier = EnvModifier(None)) extends Literal {
  def str = s"$value$envModifier"
  def children = envModifier :: Nil
}

case class DictLiteral(axis: Identifier, assignments: Map[String, Expr]) extends Literal {
  def str = s"{$axis: ${assignments.map { case (k, v) => s"$k=$v" }.mkString(" ")}}"
  def children = Iterable(axis) ++ assignments.values
}

sealed trait KeySelector extends Node

case class Key1(key: String) extends KeySelector {
  def str = key
  def children = Nil
}

sealed trait KeyN extends KeySelector

case class Keys(keys: Set[String]) extends KeyN {
  def str = keys.mkString("[", " ", "]")
  def children = Nil
}

case class Star() extends KeyN {
  def str = "*"
  def children = Nil
}

case class Index1(axis: Identifier, key: Key1) extends Node {
  def str = s"$axis: $key"
  def children = axis :: key :: Nil
}

case class IndexN(axis: Identifier, keys: KeyN) extends Node {
  def str = s"$axis: $keys"
  def children = axis :: keys :: Nil
}

case class Indices1(indices: Seq[Index1]) extends MapWrapper[Identifier, Key1] with Node {
  def str = if (indices.isEmpty) "" else s"[${indices.mkString(", ")}]"
  def children = indices
  def underlying = orderedMap(indices.map { case Index1(axis, key) => (axis, key) })
}

case class IndicesN(indices: Seq[IndexN]) extends MapWrapper[Identifier, KeyN] with Node {
  def str = if (indices.isEmpty) "" else s"[${indices.mkString(", ")}]"
  def children = indices
  def underlying = orderedMap(indices.map { case IndexN(axis, keys) => (axis, keys) })
}

case class TaskRef1(name: Identifier, indices: Indices1) extends Node {
  def str = s"$name$indices"
  def children = Iterable(name) ++ indices.flatMap { case (k, v) => Iterable(k, v) }
}

case class TaskRefN(name: Identifier, indices: IndicesN) extends Node {
  def str = s"$name$indices"
  def children = Iterable(name) ++ indices.flatMap { case (k, v) => Iterable(k, v) }
}

case class ValRef(name: Identifier) extends Expr {
  def str = s"$$$name"
  def children = name :: Nil
}

case class TaskOutputRef1(task: TaskRef1, name: Identifier) extends Expr {
  def str = s"$$$task.$name"
  def children = task :: name :: Nil
}

case class TaskOutputRefN(tasks: TaskRefN, name: Identifier) extends Expr {
  def str = s"$$$tasks.$name"
  def children = tasks :: name :: Nil
}

case class Parameter(name: Identifier, env: EnvModifier) extends Node {
  def str = s"$name$env"
  override def children = name :: env :: Nil
}

sealed trait Assignment extends Node

case class ExplicitAssignment(param: Parameter, value: Expr) extends Assignment {
  def str = s"$param = $value"
  def children = Iterable(param, value)
}

case class RefAssignment(param: Parameter) extends Assignment {  // param = $
  def str = s"$param = $$${param.name}"
  def children = Iterable(param)
}

case class SameNameAssignment(param: Parameter) extends Assignment {  // out [= "out"]
  def str = s"""$param = "${param.name}""""
  def children = Iterable(param)
}

case class Assignments(assignments: Seq[Assignment]) extends MapWrapper[Identifier, (EnvModifier, Expr)] with Node {
  def str = assignments.mkString(", ")
  def children = assignments

  val underlying = orderedMap(assignments.map {
    case ExplicitAssignment(param, v) => (param.name, (param.env, v))
    case RefAssignment(param)         => (param.name, (param.env, ValRef(param.name)))
    case SameNameAssignment(param)    => (param.name, (param.env, StringLiteral(param.name.name, param.env)))
  })
}

case class FuncCall(funcName: Identifier, inputs: Assignments) extends Node {
  def str = s"$funcName($inputs)"
  def children = Iterable(funcName, inputs)
}

case class DecoratorCall(call: FuncCall) extends Node {
  def str = s"@$call\n"
  def children = Iterable(call)
}

case class DecoratorCalls(calls: Seq[DecoratorCall]) extends Node {
  def str = calls.mkString("")
  def children = calls
}

sealed trait Impl extends Node

case class ScriptImpl(script: Verbatim) extends Impl {
  def str = s":\n$script"
  def children = Iterable(script)
}

case class FuncCallImpl(call: FuncCall) extends Impl {
  def str = s" = $call"
  def children = Iterable(call)
}

sealed trait Statement extends Node

case class ValDef(name: Identifier, value: Expr) extends Statement {
  def str = s"$name = $value"
  def children = Iterable(name, value)
}

case class TaskDef(decorators: DecoratorCalls, name: Identifier, env: EnvModifier, inputs: Assignments, outputs: Assignments, impl: Impl) extends Statement {
  def str = s"${decorators}task $name$env($inputs) -> ($outputs)$impl"
  def children = decorators.calls ++ Iterable(env, name, inputs, outputs, impl)
}

case class ServiceDef(decorators: DecoratorCalls, name: Identifier, env: EnvModifier, inputs: Assignments, impl: Impl) extends Statement {
  def str = s"${decorators}service $name($inputs)$impl"
  def children = decorators.calls ++ Iterable(name, inputs, impl)
}

case class PackageDef(decorators: DecoratorCalls, name: Identifier, inputs: Assignments, impl: Impl) extends Statement {
  def str = s"${decorators}package $name($inputs)$impl"
  def children = decorators.calls ++ Iterable(name, inputs, impl)
}

case class FuncDef(name: Identifier, params: Assignments, input: Identifier, inputName: StringLiteral, impl: Impl) extends Statement {
  def str = s"""def $name($params) <- ($input = "$inputName")$impl"""
  def children = Iterable(name, params, input, impl)
}

case class PlanDef(name: Identifier, taskRefs: Seq[TaskRefN]) extends Statement {
  def str = s"plan $name = {${taskRefs.mkString(" ")}}"
  def children = Iterable(name) ++ taskRefs
}

case class ImportStatement(fileName: String) extends Statement {
  def str = s"import $fileName"
  def children = Nil
}
