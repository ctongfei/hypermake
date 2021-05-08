package hypermake.cli

import fansi._
import hypermake.execution.Status
import hypermake.semantics.Job


trait DisplayRenderer {
  def job(j: Job, status: Status): String
}

object DisplayRenderer {

  object Powerline extends DisplayRenderer {
    def colors(status: Status) = status match {
      case Status.Pending => (Back.Black, Color.DarkGray)
      case Status.Running => (Back.LightBlue, Color.White)
      case Status.Succeeded => (Back.Green, Color.DarkGray)
      case Status.Failed => (Back.LightRed, Color.White)
    }

    def job(j: Job, status: Status) = {
      val (bg, fg) = colors(status)
      bg(fg(s" ${Bold.On(j.name.name)}  ${j.argsString} ")).toString()
    }
  }

}
