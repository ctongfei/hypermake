package hypermake.core

import java.io.{File => JFile}

import better.files._
import zio._
import zio.process.{Command, ProcessOutput}

import hypermake.util._

/** Encapsulates how a script file is executed. */
trait Runner {

  def run(scriptFile: String, wd: String)(implicit ctx: Context, std: StdSinks): HIO[ExitCode]

}

object Runner {

  /**
   * A synchronous local runner that executes scripts by starting a subprocess
   * and waiting for it to complete.
   */
  object POpen extends Runner {

    def run(scriptFile: String, wd: String)(implicit ctx: Context, std: StdSinks): HIO[ExitCode] = {
      val shell = ctx.runtime.shell
      val interpreter :: interpreterArgs = shell.split(' ').toList
      for {
        process <- Command(interpreter, (interpreterArgs :+ scriptFile): _*)
          .workingDirectory(new JFile(wd).getAbsoluteFile)
          .env(ctx.runtime.envVars)
          .stderr(ProcessOutput.Pipe)
          .stdout(ProcessOutput.Pipe)
          .run
        _ <- process.stdout.stream.run(std.out) <&> process.stderr.stream.run(std.err)
        exitCode <- process.exitCode
      } yield exitCode
    }
  }
}

trait AsyncRunner extends Runner {

  def refreshDuration(implicit ctx: Context): Duration

  def submit(scriptFile: String, wd: String)(implicit ctx: Context, std: StdSinks): HIO[String] // returns the pid
  def poll(pid: String)(implicit ctx: Context, std: StdSinks): HIO[Option[ExitCode]]
  def cancel(pid: String)(implicit ctx: Context, std: StdSinks): HIO[Unit]

  def run(scriptFile: String, wd: String)(implicit ctx: Context, std: StdSinks): HIO[ExitCode] = {
    for {
      pid <- submit(scriptFile, wd)
      exitCode <- poll(pid).delay(refreshDuration).repeatUntil(_.isDefined)
    } yield exitCode.get
  }
}

object AsyncRunner {

  /**
   * A custom async runner from an object definition (e.g., nohup, tmux session).
   * Looks up `${name}.submit`, `${name}.poll`, and `${name}.cancel` from the .hm definitions.
   */
  class Custom(val runnerName: String) extends AsyncRunner {

    import FileSys._
    import zio.stream.ZSink

    def refreshDuration(implicit ctx: Context): Duration =
      getValueByNameOpt(s"$runnerName.refresh_duration")
        .map(_.toInt)
        .getOrElse(5)
        .seconds

    def submit(host: Host, scriptFile: String, wd: String)(implicit ctx: Context, std: StdSinks): HIO[String] = {
      for {
        _ <- getScriptByName(s"$runnerName.submit")
          .withArgs("_internal_script" -> scriptFile, "wd" -> wd)
          .executeLocally(wd)(ctx.runtime, StdSinks.devNull)
        pid <- host.read(s"$wd${host./}pid")
      } yield pid.trim
    }

    def poll(pid: String)(implicit ctx: Context, std: StdSinks): HIO[Option[ExitCode]] = {
      val baos = new java.io.ByteArrayOutputStream()
      val sinks = StdSinks(
        out = ZSink.fromOutputStream(baos),
        err = ZSink.drain.map(_ => 0L)
      )
      for {
        _ <- getScriptByName(s"$runnerName.poll")
          .withArgs("pid" -> pid)
          .executeLocally(ctx.runtime.workDir)(ctx.runtime, sinks)
        output = baos.toString.trim
      } yield if (output.isEmpty) None else Some(ExitCode(output.toInt))
    }

    def cancel(pid: String)(implicit ctx: Context, std: StdSinks): HIO[Unit] = for {
      _ <- getScriptByName(s"$runnerName.cancel")
        .withArgs("pid" -> pid)
        .executeLocally(ctx.runtime.workDir)(ctx.runtime, std)
    } yield ()

  }

}
