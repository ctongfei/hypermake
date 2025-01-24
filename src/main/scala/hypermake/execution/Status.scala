package hypermake.execution

import fansi._

sealed abstract class Status(val text: String, val symbol: Char, val color: Attr) {
  override def toString = text
  def isSuccess = this == Status.Success
}

object Status {
  case object Pending extends Status("PENDING", '•', Color.White)

  case object Waiting extends Status("WAITING", '>', Color.Yellow)

  case object Running extends Status("RUNNING", '>', Color.Blue)

  case object Complete extends Status("COMPLETE", '•', Color.DarkGray)

  sealed abstract class Result(text: String, symbol: Char, color: Attr) extends Status(text, symbol, color)

  case object Success extends Result("SUCCESS", '•', Color.Green)

  sealed abstract class Failure(reason: String) extends Result(s"FAILED: $reason", '×', Color.Red)

  object Failure {
    case class NonZeroExitCode(exitCode: Int)
        extends Failure(s"exit code $exitCode ≠ 0")

    case class MissingOutputs(missingOutputs: Set[String])
        extends Failure(s"output${if (missingOutputs.size == 1) "" else "s"} {${missingOutputs.mkString(", ")}} missing")

    case object FileSysError
        extends Failure("error encountered when checking status of outputs")
  }

}
