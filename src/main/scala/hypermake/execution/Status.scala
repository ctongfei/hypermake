package hypermake.execution

sealed abstract class Status(val text: String) {
  override def toString = text
}

object Status {
  case object Pending   extends Status("PENDING ")
  case object Locked    extends Status("LOCKED  ")
  case object Running   extends Status("RUNNING ")
  case object Succeeded extends Status("SUCCESS ")
  case object Failed    extends Status("FAILED  ")
  case object Complete  extends Status("COMPLETE")
}
