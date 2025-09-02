package hypermake.exception

import scala.collection._

import fastparse._

import hypermake.collection._
import hypermake.core._
import hypermake.syntax.ast.ValRef

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

case class ParametersUnboundException(names: Set[String], func: Option[String] = None)
    extends Exception(s"Parameters {${names.mkString(", ")}} are unbound${func.fold("")(fn => s" in “$fn”")}.")

case class UndefinedException(kind: String, name: String) extends Exception(s"$kind “$name” not defined.")

case class ReferenceResolutionException(ref: ValRef) extends Exception(s"Failed to resolve reference “$ref”.")

case class OutputNotDefinedException(name: String, task: Task)
    extends Exception(s"Output “$name” not defined in task “$task”.")

case class AmbiguousOutputException(task: PointedTaskTensor)
    extends Exception(s"There are more than one output of task “$task”: it is unclear which one is specified.")

case class CyclicPipelineException() extends Exception("Cyclic pipeline detected.")

case class JobFailedException(j: Job) extends Exception(s"Job ${j.colorfulString} failed.")

case class DataTransferFailedException(src: String, name: String)
    extends Exception(s"Failed to transfer file “$name” from “$src”.")

case class ObjectIsNotDecoratorException(obj: Obj)
    extends Exception(
      s"Object “$obj” cannot be used as a decorator since it does not have a unary member function “run”."
    )
// TODO: should be obj.name

case class ObjectIsNotServiceException(obj: Obj)
    extends Exception(
      s"Object “$obj” cannot be used as a service since it does not have member tasks “start” and “stop”."
    )

case class PackageOutputException(n: String) extends Exception(s"Package “$n” can only have exactly 1 output.")

case class PackageInputException(n: String)
    extends Exception(s"Inputs of package “$n” cannot be file system-dependent.")
