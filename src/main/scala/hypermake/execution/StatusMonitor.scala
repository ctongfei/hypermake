package hypermake.execution

import fansi._
import hypermake.semantics.Job
import hypermake.util._
import org.jline.terminal._
import zio._
import zio.console._

class StatusMonitor(jobs: IndexedSeq[Job]) {

  val index = jobs.view.zipWithIndex.toMap

  val terminal = TerminalBuilder.builder().dumb(true).build()
  def width = terminal.getWidth
  def height = terminal.getHeight

  val maxJobIdLength = jobs.map(_.id.length).max
  val maxJobIdDisplayLength = width - 15
  val jobsToDisplay = jobs take (height - 1)

  def renderJobName(j: Job): String =
    s"${Back.DarkGray(Bold.On(" " + j.name.name + " "))} [${Back.LightGray(j.argsString)}] "

  def renderStatus(s: Status): String = s match {
    case Status.Pending   => Color.LightGray("PENDING").render
    case Status.Locked    => Color.Yellow("LOCKED ").render
    case Status.Running   => Color.LightBlue("RUNNING").render
    case Status.Succeeded => Color.LightGreen("SUCCESS").render
    case Status.Failed    => Color.Red("FAILED ").render
  }

  def moveCursorUp(n: Int) = if (n > 0) putStr(s"\u001b[${n}A\r") else putStr("\r")
  def moveCursorDown(n: Int) = if (n > 0) putStr(s"\u001b[${n}B\r") else putStr("\r")

  def initialize: HIO[Unit] = for {
    _ <- putStrLn("")
    _ <- ZIO.foreach_(jobsToDisplay)(j => putStrLn(s"${renderJobName(j)} ${renderStatus(Status.Pending)}"))
    _ <- moveCursorUp(jobsToDisplay.length)
  } yield ()

  def updateStatus(job: Job, status: Status): HIO[Unit] = {
    val i = index(job)
    for {
      _ <- moveCursorDown(i)
      _ <- putStr(s"${renderJobName(job)} ${renderStatus(status)}")
      _ <- moveCursorUp(i)
    } yield ()
  }

  def tearDown: HIO[Unit] = ZIO.foreach_(jobsToDisplay)(_ => putStrLn(""))

}
