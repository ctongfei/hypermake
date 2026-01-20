package hypermake.core

import java.nio.file.{Files => JFiles, Paths}
import scala.collection._
import scala.util.{Random => SRandom}

import better.files._
import zio._
import zio.process._
import zio.stream.ZSink

import hypermake.util.Escaper.Shell
import hypermake.util._
import hypermake.util.logging._

/**
 * A Host is a FileSys that can execute commands (e.g., local machine, SSH, Kubernetes pod).
 * Pure storage systems like AWS S3 or Azure Blob are FileSys but not Host.
 */
trait Host extends FileSys {

  /** Output root on this file system. Intermediate results will be stored in this directory. */
  def root: String

  def tempScriptPath: String = s"$root${/}.temp_script"

  // TODO: should we allow a lightweight mode that directly runs the command string as in `bash -c "..."`?
  // TODO: this could result in quoting hell
  /** The shell to use on this host. */
  def shell: String

  /** Resolves a path relative to the given root directory. */
  def resolvePath(s: String, r: String = root) = {
    val p = s.trim
    if (p startsWith separator.toString) p
    else r + separator + p
  }

  def prepareInput(name: String, x: Value, wd: String)(implicit ctx: Context, std: StdSinks): HIO[Option[String]] =
    x match {
      case Value.Pure(_) => ZIO.none // do nothing
      case Value.Input(path, fs) =>
        if (fs == this)
          link(path, s"$wd${/}$name") as Some(resolvePath(path)) // for debugging purposes
        else
          copyFrom(path, fs, s"$wd${/}$name") as Some(resolvePath(s"$wd${/}$name"))
      case Value.Output(path, fs, job, _) =>
        if (fs == this)
          link(s"${job.path}${/}$path", s"$wd${/}$name") as Some(resolvePath(s"${job.path}${/}$path"))
        else
          copyFrom(s"${job.path}${/}$path", fs, s"$wd${/}$name") as Some(resolvePath(s"$wd${/}$name"))
      case Value.Multiple(values, _) =>
        for {
          dir <- mkdir(s"$wd${/}$name")
          r <- ZIO.foreachPar(values.allPairs) { case (c, v) =>
            prepareInput(ctx.percentEncodedCaseStringPath(c), v, resolvePath(s"$wd${/}$name"))
          }
        } yield Some(resolvePath(s"$wd${/}$name"))
    }

  /** Executes a command with arguments and environment variables, and returns the exit code. */
  def execute(command: String, args: Seq[String], wd: String, envArgs: Map[String, String])(implicit
      std: StdSinks
  ): HIO[ExitCode]

  /** Executes a script file with environment variables. */
  def executeFile(scriptFile: String, wd: String, envArgs: Map[String, String])(implicit
      std: StdSinks
  ): HIO[ExitCode]

  /** Executes a [[Script]] object. */
  def executeScript(script: Script, wd: String)(implicit std: StdSinks): HIO[ExitCode] = {
    val hexString = f"${SRandom.nextInt()}%08x"
    val tempScriptFile = s"$tempScriptPath${/}$hexString"
    for {
      _ <- mkdir(tempScriptPath)
      _ <- write(tempScriptFile, script.script)
      u <- executeFile(wd, tempScriptFile, script.args.toStrMap)
    } yield u
  }

}

object Host {

  def apply(name: String)(implicit ctx: Context): Host = {
    if (name == "local" || name == "")
      ctx.local
    else if (ctx.hostTable contains name)
      ctx.hostTable(name)
    else {
      val host = new Host.Custom(name)
      ctx.hostTable += name -> host
      host
    }
  }

  def local(implicit ctx: Context): Host = ctx.local

  /** The local host (where HyperMake runs). */
  class Local(implicit ctx: Context) extends FileSys.Local with Host {

    import ctx._
    val shell = ctx.root.values.get("local.shell").map(_.default.value).getOrElse("bash")

    val root = {
      val relRoot = ctx.root.values.get("local.root").map(_.default.value).getOrElse("out")
      resolvePath(relRoot, ctx.runtime.workDir)
    }

    override def execute(
        command: String,
        args: Seq[String],
        wd: String,
        envArgs: Map[String, String]
    )(implicit
        std: StdSinks
    ) = {
      val interpreter :: interpreterArgs = command.split(' ').toList
      val resolvedWd = resolvePath(wd)
      val pb = zio.process.Command(interpreter, (interpreterArgs ++ args): _*)
        .workingDirectory(File(resolvedWd).toJava.getAbsoluteFile)
        .env(envArgs.toMap)
        .stderr(ProcessOutput.Pipe)
        .stdout(ProcessOutput.Pipe)
      for {
        process <- pb.run
        _ <- process.stdout.stream.run(std.out) <&> process.stderr.stream.run(std.err)
        exitCode <- process.exitCode
        _ <- write(s"$wd${/}exitcode", exitCode.code.toString)
      } yield exitCode
    }

    def executeFile(scriptFile: String, wd: String, envArgs: Map[String, String])(implicit
        std: StdSinks
    ): HIO[ExitCode] =
      for {
        exitCode <- execute(shell, Seq(scriptFile), wd, envArgs)
        _ <- logExitCode(scriptFile)(exitCode)
      } yield exitCode
  }

  /** A custom host from an object definition (e.g., SSH, Kubernetes pod). */
  class Custom(val hostName: String)(implicit ctx: Context) extends FileSys.Custom(hostName) with Host {

    import FileSys._
    import ctx.runtime

    val shell = getValueByName(s"${hostName}.shell")

    def execute(command: String, args: Seq[String], wd: String, envVars: Map[String, String])(implicit
        std: StdSinks
    ) = {
      val script = getScriptByName(s"${hostName}.execute")
        .withArgs(
          "command" ->
            s"""cd ${resolvePath(wd)};
               |env ${Shell.escapeKeyValuePairs(envVars)} $command ${args.mkString(" ")}
               |""".stripMargin.replace("\n", "")
        )
      ctx.local.executeScript(script, wd)
    }

    def executeFile(scriptFile: String, wd: String, envArgs: Map[String, String])(implicit
        std: StdSinks
    ): HIO[ExitCode] =
      execute(shell, Seq(scriptFile), wd, envArgs)
  }

}
