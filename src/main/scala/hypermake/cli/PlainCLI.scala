package hypermake.cli

import java.nio.file._
import zio._
import zio.console._
import zio.stream._
import hypermake.core.Job
import hypermake.execution.{RuntimeConfig, Status}
import hypermake.util._
import hypermake.util.printing.Style

import java.io.{FilterOutputStream, OutputStream}
import java.nio.charset.StandardCharsets


class PrefixedOutputStream(os: OutputStream, prefix: String) extends FilterOutputStream(os) {

  @volatile private[this] var atStartOfLine: Boolean = true

  override def write(b: Int): Unit = os.synchronized {
    if (atStartOfLine)
      os.write(prefix.getBytes(StandardCharsets.UTF_8))
    os.write(b)
    atStartOfLine = b == '\n' || b == '\r'
    if (atStartOfLine) os.flush()
  }

  override def write(b: Array[Byte], off: Int, len: Int): Unit = os.synchronized {
    val lines = new String(b, off, len, StandardCharsets.UTF_8).split("(?<=[\\r\\n])")
    // split that includes concluding delimiters with positive lookbehind regex
    for (line <- lines) {
      if (atStartOfLine) os.write(prefix.getBytes(StandardCharsets.UTF_8))
      os.write(line.getBytes(StandardCharsets.UTF_8))
      atStartOfLine = line.endsWith("\n") || line.endsWith("\r")
      os.flush()
    }
  }

}

class PlainCLI(style: Style, runtime: RuntimeConfig) extends CLI.Service {

  def setup = ZIO.succeed()

  /**
   * Returns two sinks for consuming the standard output (stdout) and the standard error (stderr) streams.
   */
  def getSinks(job: Job) = IO {
    val os = ZSink.fromOutputStream(new PrefixedOutputStream(StandardStreams.out, style.render(job)))
    val es = ZSink.fromOutputStream(new PrefixedOutputStream(StandardStreams.err, style.render(job)))
    val ofs = ZSink.fromFile(Paths.get(job.absolutePath, "stdout"))
    val efs = ZSink.fromFile(Paths.get(job.absolutePath, "stderr"))
    if (runtime.silent)
      (ofs, efs)
    else ((os zipWithPar ofs)((a, _) => a), (es zipWithPar efs)((a, _) => a))
  }

  def println(s: String) = if (runtime.silent) ZIO.succeed() else putStrLn(s)

  def show(job: Job, status: Status) = ZIO.succeed(style.render(job, status))

  def showInGraph(job: Job, status: Status) = ZIO.succeed(style.renderInGraph(job, status))

  def update(job: Job, status: Status) = putStrLn(style.render(job, status))

  def ask: HIO[Boolean] = for {
    _ <- putStr("Continue? [y/n]: ")
    response <- getStrLn
  } yield response.trim.toLowerCase == "y"

  def teardown = ZIO.succeed()
}

object PlainCLI {

  def create(style: Style = Style.Powerline)(implicit runtime: RuntimeConfig): HIO[Managed[Throwable, PlainCLI]] = IO {
    Managed.succeed(new PlainCLI(style, runtime))
  }

}
