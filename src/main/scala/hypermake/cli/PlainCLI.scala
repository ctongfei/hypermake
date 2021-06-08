package hypermake.cli

import java.nio.file._
import zio._
import zio.console._
import zio.stream._
import hypermake.core.Job
import hypermake.execution.{RuntimeContext, Status}
import hypermake.util.StandardStreams.PrefixedOutputStream
import hypermake.util._
import hypermake.util.printing.Style

class PlainCLI(style: Style, runtime: RuntimeContext) extends CLI.Service {

  def initialize = ZIO.succeed()

  /**
   * Returns two sinks for consuming the standard output (stdout) and the standard error (stderr) streams.
   */
  def getSinks(job: Job) = IO {
    val os = ZSink.fromOutputStream(new PrefixedOutputStream(StandardStreams.out, style.render(job)))
    val es = ZSink.fromOutputStream(new PrefixedOutputStream(StandardStreams.err, style.render(job)))
    val ofs = ZSink.fromFile(Paths.get(job.absolutePath, "stdout"))
    val efs = ZSink.fromFile(Paths.get(job.absolutePath, "stderr"))
    if (runtime.silent) (ofs, efs) else ((os zipWithPar ofs)(_+_), (es zipWithPar efs)(_+_))
  }

  def println(s: String) = if (runtime.silent) ZIO.succeed() else putStrLn(s)

  def update(job: Job, status: Status) = putStrLn(style.render(job, status))

  def ask: HIO[Boolean] = for {
    _ <- putStr("Continue? [y/n]: ")
    response <- getStrLn
  } yield response.trim.toLowerCase == "y"

  def tearDown = ZIO.succeed()
}

object PlainCLI {

  def create(style: Style = Style.Powerline)(implicit runtime: RuntimeContext): HIO[Managed[Throwable, PlainCLI]] = IO {
    Managed.succeed(new PlainCLI(style, runtime))
  }

}
