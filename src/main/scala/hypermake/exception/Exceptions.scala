package hypermake.exception

import fastparse._
import hypermake.collection._
import hypermake.core._

import scala.collection._

case class ParsingException(failure: Parsed.Failure) extends Exception(s"Parsing error: \n${failure.trace().longMsg}")

case class ValueNotPureException(value: String) extends Exception(s"Value “$value” is not pure.")

case class AxesAlignmentException(name: Axis, v1: Seq[String], v2: Seq[String])
    extends Exception(
      s"Axis “$name” does not align in two occurrences: {${v1.mkString(", ")}} != {${v2.mkString(", ")}}"
    )

case class AxesDefaultKeyMismatchException(name: Option[Axis], p1: String, p2: String)
    extends Exception(s"Default keys of ${name.fold("axis")(a => f"axis “$a”")} do not match: “$p1” != “$p2”")

case class AxisKeyNotFoundException(axis: Axis, key: String)
    extends Exception(s"Key “$key” not found in axis ${axis.name}.")

case class DuplicateDefinitionException(kind: String, name: String)
    extends Exception(s"$kind “$name” cannot be defined twice.")

case class ParametersUnboundException(names: Set[String], func: String)
    extends Exception(s"Parameters {${names.mkString(", ")}} unbound in “$func”.")

case class UndefinedException(kind: String, name: String) extends Exception(s"$kind “$name” not defined.")

case class OutputNotDefinedException(name: String, task: Task)
    extends Exception(s"Output “$name” not defined in task “$task”.")

case class AmbiguousOutputException(task: PointedTaskTensor)
    extends Exception(s"There are more than one output of task “$task”: it is unclear which one is specified.")

case class NonStringForOutputException(output: String)
    extends Exception(s"Only string literals are allowed for the value of an output “$output”.")

case class CyclicWorkflowException() extends Exception(s"Cyclic workflow detected.")

case class JobFailedException(j: Job) extends Exception(s"Job ${j.colorfulString} failed.")

case class DataTransferFailedException(src: String, name: String)
    extends Exception(s"Failed to transfer file $name from “$src”.")

case class ObjectIsNotDecoratorException(obj: Obj)
    extends Exception(
      s"Object “$obj” cannot be used as a decorator since it does not have a unary member function “apply”."
    )
    // TODO: should be obj.name
