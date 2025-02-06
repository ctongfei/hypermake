package hypermake.execution

import fansi._
import upickle.default._

// TODO: split into 2 types; one for display before running; the other for display after querying
sealed abstract class Status(val text: String, val symbol: Char, val color: Attr) {
  override def toString = text
  def isSuccess = this == Status.Success

  def describe: Status.Description
}

object Status {
  case class Description(status: String, failureReason: Option[String])
  object Description {
    implicit val rw: ReadWriter[Description] = macroRW
  }

  case object Pending extends Status("PENDING", '•', Color.White) {
    def describe = Description("pending", None)
  }

  case object Waiting extends Status("WAITING", '>', Color.Yellow) {
    def describe = Description("waiting", None)
  }

  case object Running extends Status("RUNNING", '>', Color.Blue) {
    def describe = Description("running", None)
  }

  case object Complete extends Status("COMPLETE", '•', Color.DarkGray) {
    def describe = Description("complete", None)
  }

  sealed abstract class Result(text: String, symbol: Char, color: Attr) extends Status(text, symbol, color)

  case object Success extends Result("SUCCESS", '•', Color.Green) {
    def describe = Description("success", None)
  }

  sealed abstract class Failure(reason: String) extends Result(s"FAILED: $reason", '×', Color.Red) {
    def describe = Description("failure", Some(reason))
  }

  object Failure {
    case class NonZeroExitCode(exitCode: Int)
        extends Failure(s"exit code $exitCode ≠ 0")

    case class MissingOutputs(missingOutputs: Set[String])
        extends Failure(s"output${if (missingOutputs.size == 1) "" else "s"} {${missingOutputs.mkString(", ")}} missing")

    case object FileSysError
        extends Failure("error encountered when checking status of outputs")
  }

}
