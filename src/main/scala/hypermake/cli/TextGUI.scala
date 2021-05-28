package hypermake.cli

import com.googlecode.lanterna.TerminalSize
import com.googlecode.lanterna.gui2._
import com.googlecode.lanterna.screen.Screen
import com.googlecode.lanterna.terminal._
import zio._
import zio.stream._
import hypermake.core._
import hypermake.execution.Status
import hypermake.util._
import hypermake.util.printing.Style


class TextGUI(val screen: Screen, val jobs: IndexedSeq[Job], semaphore: Semaphore, style: Style = Style.Powerline) extends CLI {

  val gui = new MultiWindowTextGUI(screen)
  val windowManager = new DefaultWindowManager()
  val topWindow = new BasicWindow("Hypermake")
  topWindow.setFixedSize(new TerminalSize(screen.getTerminalSize.getColumns, jobs.size))
  val panel = new Panel(new LinearLayout(Direction.VERTICAL))
  topWindow.setComponent(panel)

  val index = jobs.view.zipWithIndex.toMap
  val components = jobs.map { j => new TextBox(style.render(j, Status.Pending)) }
  components.foreach(panel.addComponent)
  gui.addWindowAndWait(topWindow)

  def initialize: HIO[Unit] = ZIO.succeed()

  def getSinks(job: Job): HIO[(HSink[Byte], HSink[Byte])] = for {
    window <- IO { new BasicWindow(job.id) }
    box <- IO {
      val box = new TextBox("", TextBox.Style.MULTI_LINE)
      window.setComponent(box)
      box
    }
    _ <- IO { gui.addWindow(window) }
   // _ <- stream.foreach(l => IO { box.addLine(l) }) // TODO: ZSink
    u <- IO { gui.removeWindow(window) }
  } yield (null, null)

  def update(job: Job, status: Status): HIO[Unit] = semaphore.withPermit {
    for {
      _ <- IO { components(index(job)).setText(style.render(job, status)) }
    } yield ()
  }

}

object TextGUI {

  def create(jobs: IndexedSeq[Job], semaphore: Semaphore): Managed[Throwable, TextGUI] = {
    val acquire = IO {
      val screen = new DefaultTerminalFactory().createScreen()
      screen.startScreen()
      new TextGUI(screen, jobs, semaphore)
    }
    val release = (cli: TextGUI) => IO { cli.screen.stopScreen() }.orElse(ZIO.unit)
    val screen = Managed.make(acquire)(release)
    screen
  }

}