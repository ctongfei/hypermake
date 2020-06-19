package forge.lang

import scala.collection._

object AST {

  sealed trait Node { self =>
    def str: String
    def children: Iterable[Node]
    def recursiveChildren: Iterable[Node] = new Iterable[Node] {
      val stack = mutable.Stack(self)
      def iterator: Iterator[Node] = new Iterator[Node] {
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

  sealed trait Value extends Node

  case class Identifier(name: String) extends Value {
    def str = name
    def children = Nil
  }

  case class InlineCommand(command: String) extends Node {
    def str = command
    def children = Nil
  }

  case class Script(script: String) extends Node {
    def str = script.split('\n').map(l => "  " + l).mkString("\n")
    def children = Nil
  }

  sealed trait Literal extends Value
  case class StringLiteral(value: String) extends Literal {
    def str = s"""\"$value\""""
    def children = Nil
  }
  case class ShellLiteral(command: String) extends Literal {
    def str = command
    def children = Nil
  }

  case class DictLiteral(axis: Identifier, assignments: Map[StringLiteral, Value]) extends Literal {
    def str = s"{$axis: ${assignments.map { case (k, v) => s"$k=$v"}.mkString(" ")}}"
    def children = Iterable(axis) ++ assignments.flatMap { case (k, v) => Iterable(k, v) }
  }

  sealed trait KeySelector extends Node
  case class Keys(keys: Set[StringLiteral]) extends KeySelector {
    def str = keys.mkString(" ")
    def children = keys
  }
  case class Star() extends KeySelector {
    def str = "*"
    def children = Nil
  }

  case class Index(axis: Identifier, keys: KeySelector) extends Node {
    def str = s"$axis: $keys"
    def children = axis :: keys :: Nil
  }

  case class TaskRef(taskName: Identifier, indices: Map[Identifier, KeySelector]) extends Node {
    def str = s"$taskName[${indices.map { case (i, ks) => s"$i: $ks" }.mkString(", ")}]"
    def children = Iterable(taskName) ++ indices.flatMap { case (k, v) => Iterable(k, v) }
  }

  case class ValRef(id: Identifier, task: Option[TaskRef]) extends Value {
    def str = s"$$$id${task.fold("")(t => s"@$t")}"
    def children = id :: task.toList
  }

  sealed trait Assignment extends Node
  case class ExplicitAssignment(id: Identifier, value: Value) extends Assignment with Stmt {
    def str = s"$id = $value"
    def children = Iterable(id, value)
  }
  case class RefAssignment(id: Identifier) extends Assignment {
    def str = s"$id = $$$id"
    def children = Iterable(id)
  }

  def mapToString(map: Map[Identifier, Value], sep: String) = map.map { case (k, v) => s"$k=$v" }.mkString(sep)

  case class FuncCall(funcName: Identifier, inputs: Map[Identifier, Value]) extends Node {
    def str = s"$funcName(${mapToString(inputs, ", ")})"
    def children = Iterable(funcName) ++ inputs.flatMap { case (k, v) => Iterable(k, v) }
  }
  case class Decorator(call: FuncCall) extends Node {
    def str = s"@$call\n"
    def children = Iterable(call)
  }

  sealed trait Impl extends Node
  case class ScriptImpl(script: Script) extends Impl {
    def str = s":\n$script"
    def children = Iterable(script)
  }
  case class FuncCallImpl(call: FuncCall) extends Impl {
    def str = s"= $call"
    def children = Iterable(call)
  }

  sealed trait Stmt extends Node
  case class FuncDef(decorator: Option[Decorator], funcName: Identifier, inputs: Set[Identifier], outputs: Set[Identifier], impl: Impl) extends Stmt {
    def str = s"${decorator.fold("")(_.toString)}def $funcName(${inputs.mkString(", ")}) -> (${outputs.mkString(", ")}) $impl"
    def children = decorator.toList ++ Iterable(funcName) ++ inputs ++ outputs ++ Iterable(impl)
  }

  case class TaskDef(decorator: Option[Decorator], taskName: Identifier, inputs: Map[Identifier, Value], outputs: Map[Identifier, Value], impl: Impl) extends Stmt {
    def str = s"${decorator.fold("")(_.toString)}task $taskName(${mapToString(inputs, ", ")}) -> (${mapToString(outputs, ", ")}) $impl"
    def children = decorator.toList ++ Iterable(taskName) ++ inputs.flatMap { case (k, v) => Iterable(k, v) } ++ outputs.flatMap { case (k, v) => Iterable(k, v) } ++ Iterable(impl)
  }

  case class ServiceDef(decorator: Option[Decorator], serviceName: Identifier, inputs: Map[Identifier, Value], outputs: Map[Identifier, Value], impl: Impl) extends Stmt {
    def str = s"${decorator.fold("")(_.toString)}service $serviceName(${mapToString(inputs, ", ")}) -> (${mapToString(outputs, ", ")}) $impl"
    def children = decorator.toList ++ Iterable(serviceName) ++ inputs.flatMap { case (k, v) => Iterable(k, v) } ++ outputs.flatMap { case (k, v) => Iterable(k, v) } ++ Iterable(impl)
  }

  case class PackageDef(decorator: Option[Decorator], packageName: Identifier, inputs: Map[Identifier, Value], impl: Impl) extends Stmt {
    def str = s"${decorator.fold("")(_.toString)}package $packageName(${inputs.mkString(", ")}) $impl"
    def children = decorator.toList ++ Iterable(packageName) ++ inputs.flatMap { case (k, v) => Iterable(k, v) } ++ Iterable(impl)
  }

  case class DecoratorDef(decoratorName: Identifier, inputs: Set[Identifier], command: Identifier, outputs: Set[Identifier], impl: Impl) extends Stmt {
    def str = s"def $decoratorName(${inputs.mkString(", ")})($command) -> (${outputs.mkString(", ")}) $impl"
    def children = Iterable(decoratorName) ++ inputs ++ Iterable(command) ++ outputs ++ Iterable(impl)
  }

  case class PlanDef(planName: Identifier, taskRefs: Set[TaskRef]) extends Stmt {
    def str = s"plan $planName = {${taskRefs.mkString(" ")}}"
    def children = Iterable(planName) ++ taskRefs
  }

  case class ImportStmt(fileName: StringLiteral) extends Stmt {
    def str = s"import $fileName"
    def children = Iterable(fileName)
  }

}
