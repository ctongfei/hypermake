package hypermake.execution

import fansi._

sealed abstract class Status(val text: String, val symbol: Char, val color: Attr) {
  override def toString = text
}

object Status {
  case object Pending   extends Status("PENDING ", '•', Color.White )
  case object Waiting   extends Status("WAITING ", '>', Color.Yellow)
  case object Running   extends Status("RUNNING ", '>', Color.Blue)
  case object Succeeded extends Status("SUCCESS ", '•', Color.Green)
  case object Failed    extends Status("FAILED  ", '×', Color.Red)
  case object Complete  extends Status("COMPLETE", '•', Color.DarkGray)
}
