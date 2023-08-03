package hypermake.core

import java.io.{File => JFile}
import scala.collection._
import better.files._
import hypermake.cli.CLI
import zio._
import zio.process._
import hypermake.collection._
import hypermake.execution._
import hypermake.util.StandardStreams.out
import hypermake.util._
import zio.stream.ZSink


/**
 * Encapsulates a script together with its external arguments.
 */
case class Script(
                   script: String,
                   args: Map[Name, Value] = Map(),
                   outputArgs: Map[Name, Value] = Map()
                 )(implicit runtime: RuntimeConfig)
{

  def withNewArgs(newArgs: Map[Name, Value]) = Script(script, args ++ newArgs, outputArgs)

  def withArgs(newArgs: (String, String)*) =
    withNewArgs(newArgs.map { case (a, v) => Name(a) -> Value.Pure(v) }.toMap)

  def withNewOutputArgs(newArgs: Map[Name, Value]) = Script(script, args, outputArgs ++ newArgs)

  def strArgs: Map[String, String] = {
    (args).map { case (k, v) => k.name -> v.absValue } ++ outputArgs.map { case (k, v) => k.name -> v.value }
  }

  /**
   * Writes this script as a local temporary file and executes it with its arguments.
   *
   * @param workDir The working directory to run this temporary script.
   *                By default this is the working directory of the outer Hypermake process.
   */
  def executeLocally(workDir: String = runtime.workDir)(implicit std: StdSinks): HIO[Process] = {
    val tempScriptFile = runtime.tempFile(prefix = "hypermake_temp_script")
    // allows the case where the interpreter has arguments: python -v ...
    val command = s"${runtime.shell} $tempScriptFile".split(' ')

    for {
      _ <- IO {
        File(tempScriptFile).write(script)
      }
      process <- Command(command.head, command.tail: _*)
        .workingDirectory(new JFile(workDir))
        .env(strArgs.toMap ++ runtime.envVars) // Hypermake args are injected as environment vars
        .stderr(ProcessOutput.Pipe)
        .stdout(ProcessOutput.Pipe)
        .run
      _ <- process.stdout.stream.run(std.out) <&> process.stderr.stream.run(std.err)
    } yield process
  }

}
