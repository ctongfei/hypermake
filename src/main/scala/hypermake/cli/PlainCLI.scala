package hypermake.cli

import zio._
import zio.console._
import zio.stream._
import hypermake.core.Job
import hypermake.execution.Status
import hypermake.util.StandardStreams.PrefixedOutputStream
import hypermake.util._
import hypermake.util.printing.Style

class PlainCLI(style: Style) extends CLI {

  def initialize = ZIO.succeed()

  def getSinks(job: Job) = IO {
    val os = new PrefixedOutputStream(StandardStreams.out, style.render(job))
    val es = new PrefixedOutputStream(StandardStreams.err, style.render(job))
    (ZSink.fromOutputStream(os), ZSink.fromOutputStream(es))
  }

  def update(job: Job, status: Status) = putStrLn(style.render(job, status))
}

object PlainCLI {

  def create(style: Style = Style.Powerline): HIO[Managed[Throwable, PlainCLI]] = IO {
    Managed.succeed(new PlainCLI(style))
  }

}
