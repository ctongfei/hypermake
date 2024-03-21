package hypermake.util.printing

import fansi._

import hypermake.collection._
import hypermake.core._
import hypermake.execution._

trait Style {

  def bg(s: Status): Attrs
  def bgAsFg(s: Status): Attrs
  def nameFg(s: Status): Attrs
  def argsFg(s: Status): Attrs
  def name(s: Status) = bg(s) ++ nameFg(s)
  def args(s: Status) = bg(s) ++ argsFg(s)
  def render0: String
  def render(j: Job): String
  def render(j: Job, s: Status): String
  def renderInGraph(j: Job, s: Status): String

}

object Style {

  object Plain extends Style {
    def bg(s: Status) = Attrs.Empty
    def bgAsFg(s: Status) = Attrs.Empty
    def nameFg(s: Status) = Attrs.Empty
    def argsFg(s: Status) = Attrs.Empty
    def render0 = ">"
    def render(j: Job) = s"${j.colorfulString}> "
    def render(j: Job, s: Status) = s"${j.colorfulString}: ${s.text}"
    def renderInGraph(j: Job, s: Status) = s"${s.symbol} ${j.colorfulString}"
  }

  object Powerline extends Style {
    def bg(s: Status) = s match {
      case Status.Pending   => Back.DarkGray
      case Status.Waiting   => Back.Yellow
      case Status.Running   => Back.Blue
      case Status.Succeeded => Back.Green
      case Status.Failed    => Back.Red
      case Status.Complete  => Back.Cyan
    }

    def bgAsFg(s: Status) = s match {
      case Status.Pending   => Color.DarkGray
      case Status.Waiting   => Color.Yellow
      case Status.Running   => Color.Blue
      case Status.Succeeded => Color.Green
      case Status.Failed    => Color.Red
      case Status.Complete  => Color.Cyan
    }

    def nameFg(s: Status) = Bold.On ++ Color.White

    def argsFg(s: Status) = Color.LightGray

    override def render0 = "  " // TODO: background

    def render(j: Job) = {
      val b = Back.Black
      val f = Color.Black
      val a = Back.Black ++ Color.DarkGray
      val n = Back.Black ++ Bold.On ++ Color.DarkGray
      val jobName = n(j.name)
      val jobArgs = j.canonicalCase
        .map { case (k, v) =>
          a(s"$k:") ++ (a ++ Bold.On)(v)
        }
        .reduceOption(_ ++ a(" ") ++ _)
        .map(_ ++ a(" "))
        .getOrElse(n("default "))
      (b(" ") ++ jobName ++ a("  ") ++ jobArgs ++ f(" ")).render
    }

    def render(j: Job, s: Status) = {
      val b = bg(s)
      val f = bgAsFg(s)
      val a = args(s)
      val jobName = name(s)(j.name)
      val jobArgs = j.canonicalCase
        .map { case (k, v) =>
          a(s"$k:") ++ (a ++ Bold.On)(v)
        }
        .reduceOption(_ ++ a(" ") ++ _)
        .map(_ ++ a(" "))
        .getOrElse(args(s)("default "))
      val statusStr = (bgAsFg(s) ++ Bold.On)(s.text)
      (b(" ") ++ jobName ++ b("  ") ++ jobArgs ++ f(" ") ++ statusStr).render
    }

    def renderInGraph(j: Job, s: Status): String = {
      val c = s.color
      val statusName = (c ++ Bold.On)(s"${s.symbol} ${j.name}")
      val jobArgs = j.canonicalCase
        .map { case (k, v) =>
          c(s"$k:") ++ (c ++ Bold.On)(v)
        }
        .reduceOption(_ ++ c(", ") ++ _)
        .map(c("[") ++ _ ++ c("]"))
        .getOrElse(c(""))
      (statusName ++ jobArgs ++ c(": " + s.text)).render
    }
  }

}
