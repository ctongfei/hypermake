package hypermake.cli

import java.io.{FilterOutputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file._

import better.files._
import zio._
import zio.console._
import zio.stream._

import hypermake.core.Job
import hypermake.execution.{RuntimeConfig, Status}
import hypermake.util._
import hypermake.util.printing.Style

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

  def setup = ZIO.unit

  def globalSinks: StdSinks = {
    if (runtime.silent)
      StdSinks(ZSink.drain.map(_ => 0), ZSink.drain.map(_ => 0))
    else {
      val os = ZSink.fromOutputStream(new PrefixedOutputStream(StdStreams.out, style.render0))
      val es = ZSink.fromOutputStream(new PrefixedOutputStream(StdStreams.err, style.render0))
      StdSinks(os, es)
    }
  }

  /** Returns two sinks for consuming the standard output (stdout) and the standard error (stderr) streams. */
  def sinks(job: Job) = {
    val os =
      ZSink.fromOutputStream(new PrefixedOutputStream(StdStreams.out, style.render(job)))
    val es =
      ZSink.fromOutputStream(new PrefixedOutputStream(StdStreams.err, style.render(job)))
    if (!File(job.absolutePath).exists)
      File(job.absolutePath).createDirectories() // mkdir -p ${job.absolutePath}
    val ofs = ZSink.fromFile(Paths.get(job.absolutePath, "stdout"))
    val efs = ZSink.fromFile(Paths.get(job.absolutePath, "stderr"))
    if (runtime.silent)
      StdSinks(ofs, efs)
    else StdSinks((os zipWithPar ofs)((a, _) => a), (es zipWithPar efs)((a, _) => a))
  }

  def println(s: String) = putStrLn(s).unless(runtime.silent)

  def show(job: Job, status: Status) = ZIO.succeed(style.render(job, status))

  def showInGraph(job: Job, status: Status) = ZIO.succeed(style.renderInGraph(job, status))

  def update(job: Job, status: Status) = putStrLn(style.render(job, status))

  def ask: HIO[Boolean] = for {
    _ <- putStr("Continue? [y/n]: ")
    response <- getStrLn
  } yield response.trim.toLowerCase == "y"

  def teardown = ZIO.unit

}

object PlainCLI {

  def create(
      style: Style = Style.Powerline
  )(implicit runtime: RuntimeConfig): HIO[Managed[Throwable, PlainCLI]] = IO {
    Managed.succeed(new PlainCLI(style, runtime))
  }

}
