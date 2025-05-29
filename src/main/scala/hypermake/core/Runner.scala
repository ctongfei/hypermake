package hypermake.core

import zio._
import zio.duration._
import zio.process._

import hypermake.core.FileSys.local
import hypermake.execution._
import hypermake.util._

/** Encapsulates how a script file is executed. */
trait Runner {

  def execute(scriptFile: String, wd: String)(implicit ctx: Context, std: StdSinks): HIO[ExitCode]

}

trait AsyncRunner {

  def refreshDuration: Duration

  def submit(scriptFile: String, wd: String): HIO[String] // returns the pid
  def alive(pid: String): HIO[Boolean]
  def kill(pid: String): HIO[Unit]

  def execute(scriptFile: String, wd: String)(implicit ctx: Context, std: StdSinks): HIO[ExitCode] = {
    for {
      pid <- submit(scriptFile, wd)
      _ <- alive(pid).delay(refreshDuration).repeatUntilEquals(false)
      exitCode <- local.read(s"$wd${local./}exitcode").mapEffect(_.toInt)
    } yield ExitCode(exitCode)
  }
}
