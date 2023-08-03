package hypermake.execution

import fansi._
import hypermake.core.Job
import hypermake.util._
import hypermake.util.printing.Style
import org.jline.terminal._
import org.jline.utils.InfoCmp
import zio._
import zio.console._

class StatusMonitor(jobs: IndexedSeq[Job], semaphore: Semaphore, style: Style = Style.Powerline) {

  val index = jobs.view.zipWithIndex.toMap

  val terminal = TerminalBuilder.builder().dumb(true).build()

  def width = terminal.getWidth

  def height = terminal.getHeight

  val maxJobIdLength = jobs.map(_.id.length).max
  val maxJobIdDisplayLength = width - 15
  val jobsToDisplay = jobs take (height - 1)

  def moveCursorUp(n: Int) = if (n > 0) putStr(s"\u001b[${n}A\r") else putStr("\r")

  def moveCursorDown(n: Int) = if (n > 0) putStr(s"\u001b[${n}B\r") else putStr("\r")

  def initialize: HIO[Unit] = semaphore.withPermit {
    for {
      _ <- IO {
        terminal.puts(InfoCmp.Capability.clear_screen)
      }
      _ <- ZIO.foreach_(jobsToDisplay)(j => putStrLn(style.render(j, Status.Pending)))
      _ <- moveCursorUp(jobsToDisplay.length)
    } yield ()
  }

  def update(job: Job, status: Status): HIO[Unit] = semaphore.withPermit {
    val i = index(job)
    for {
      _ <- moveCursorDown(i)
      _ <- putStr(style.render(job, status))
      _ <- moveCursorUp(i)
    } yield ()
  }

  def tearDown: HIO[Unit] = semaphore.withPermit {
    ZIO.foreach_(jobsToDisplay)(_ => putStrLn(""))
  }

}
