package hypermake.exception

import scala.collection._
import fastparse._
import hypermake.collection._
import hypermake.core._
import hypermake.syntax._


case class ParsingException(failure: Parsed.Failure)
  extends Exception(s"Parsing error: \n${failure.trace().longMsg}")

case class AxesAlignmentException(name: Name, v1: Seq[String], v2: Seq[String])
  extends Exception(s"Axis “$name” does not align in two occurrences: {${v1.mkString(", ")}} != {${v2.mkString(", ")}}")

case class AxesDefaultKeyMismatchException(name: Option[Name], p1: String, p2: String)
  extends Exception(s"Default keys of ${name.fold("axis")(a => f"axis “$a”")} do not match: “$p1” != “$p2”")

case class AxisKeyNotFoundException(axis: Name, key: String)
  extends Exception(s"Key “$key” not found in axis ${axis.name}.")

case class DuplicatedDefinitionException(kind: String, name: String)
  extends Exception(s"$kind “$name” cannot be defined twice.")

case class ParametersUnboundException(names: Set[Name], func: Name)
  extends Exception(s"Parameters {${names.mkString(", ")}} unbound in “$func”.")

case class UndefinedException(kind: String, name: Name)
  extends Exception(s"$kind “$name” not defined.")

case class OutputNotDefinedException(name: String, task: Task)
  extends Exception(s"Output “$name” not defined in task “$task”.")

case class AmbiguousOutputException(task: PointedCubeTask)
  extends Exception(s"There are more than one output of task “$task”: which one is specified is unclear.")

case class NonStringForOutputException(output: String)
  extends Exception(s"Only string literals are allowed for an output “$output”.")

case class CyclicWorkflowException()
  extends Exception(s"Cyclic workflow detected.")

case class JobFailedException(j: Job)
  extends Exception(s"Job ${j.colorfulString} failed.")
