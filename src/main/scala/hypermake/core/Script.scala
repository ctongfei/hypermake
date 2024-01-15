package hypermake.core

import better.files._
import hypermake.collection._
import hypermake.execution._
import hypermake.util._
import zio._
import zio.process._

import java.io.{File => JFile}
import scala.collection._

/** Encapsulates a script together with its external arguments.
  */
case class Script(
    script: String,
    args: Map[String, Value] = Map(),
    outputArgs: Map[String, Value] = Map()
)(implicit runtime: RuntimeConfig) {

  def withNewArgs(newArgs: Map[String, Value]) = Script(script, args ++ newArgs, outputArgs)

  def withArgs(newArgs: (String, String)*) =
    withNewArgs(newArgs.toMap.mapValuesE(Value.Pure))

  def withNewOutputArgs(newArgs: Map[String, Value]) = Script(script, args, outputArgs ++ newArgs)

  def strArgs: Map[String, String] = {
    args.map { case (k, v) => k.name -> v.absValue } ++ outputArgs.map { case (k, v) => k.name -> v.value }
  }

  /** Writes this script as a local temporary file and executes it with its arguments.
    *
    * @param workDir
    *   The working directory to run this temporary script. By default this is the working directory of the outer
    *   Hypermake process.
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
