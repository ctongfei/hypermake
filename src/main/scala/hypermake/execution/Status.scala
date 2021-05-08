package hypermake.execution

sealed trait Status

object Status {
  case object Pending extends Status
  case object Running extends Status
  case object Succeeded extends Status
  case object Failed extends Status
}
