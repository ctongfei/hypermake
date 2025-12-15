package hypermake.execution

import fansi._
import org.jline.terminal._
import org.jline.utils.InfoCmp
import zio.Console.{print, printLine, _}
import zio._

import hypermake.core.Job
import hypermake.util._
import hypermake.util.printing.Style

class StatusMonitor(jobs: IndexedSeq[Job], semaphore: Semaphore, style: Style = Style.Powerline) {

  val index = jobs.view.zipWithIndex.toMap

  val terminal = TerminalBuilder.builder().dumb(true).build()

  def width = terminal.getWidth

  def height = terminal.getHeight

  val maxJobIdLength = jobs.map(_.id.length).max
  val maxJobIdDisplayLength = width - 15
  val jobsToDisplay = jobs take (height - 1)

  def moveCursorUp(n: Int) = if (n > 0) print(s"\u001b[${n}A\r") else print("\r")

  def moveCursorDown(n: Int) = if (n > 0) print(s"\u001b[${n}B\r") else print("\r")

  def initialize: HIO[Unit] = semaphore.withPermit {
    for {
      _ <- ZIO.attempt {
        terminal.puts(InfoCmp.Capability.clear_screen)
      }
      _ <- ZIO.foreachDiscard(jobsToDisplay)(j => printLine(style.render(j, Status.Pending)))
      _ <- moveCursorUp(jobsToDisplay.length)
    } yield ()
  }

  def update(job: Job, status: Status): HIO[Unit] = semaphore.withPermit {
    val i = index(job)
    for {
      _ <- moveCursorDown(i)
      _ <- print(style.render(job, status))
      _ <- moveCursorUp(i)
    } yield ()
  }

  def tearDown: HIO[Unit] = semaphore.withPermit {
    ZIO.foreachDiscard(jobsToDisplay)(_ => printLine(""))
  }

}
