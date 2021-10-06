package hypermake.core

import java.io.{File => JFile}
import scala.collection._
import better.files._
import zio._
import zio.process._
import hypermake.collection._
import hypermake.execution._
import hypermake.util._


/**
 * Encapsulates a script together with its external arguments.
 */
case class Script(
                   script: String,
                   args: Map[Name, Value] = Map(),
                   outputArgs: Map[Name, Value] = Map()
                 )(implicit runtime: RuntimeContext) {

  def withNewArgs(newArgs: Map[Name, Value]) = Script(script, args ++ newArgs, outputArgs)

  def withArgs(newArgs: (String, String)*) =
    withNewArgs(newArgs.map { case (a, v) => Name(a) -> Value.Pure(v) }.toMap)

  def strArgs: Map[String, String] = (args ++ outputArgs).map { case (k, v) => k.name -> v.absValue }

  /**
   * Writes this script as a local temporary file and executes it with its arguments.
   * @param workDir The working directory to run this temporary script.
   *                By default this is the working directory of the outer Hypermake process.
   */
  def executeLocally(workDir: String = runtime.workDir): HIO[Process] = {
    val tempScriptFile = runtime.tempFile(prefix = "hypermake_temp_script")
    // allows the case where the interpreter has arguments: python -v ...
    val command = s"${runtime.SHELL} $tempScriptFile".split(' ')

    for {
      _ <- IO { File(tempScriptFile).write(this.toString) }
      process <- Command(command.head, command.tail: _*)
        .workingDirectory(new JFile(workDir))
        .env(strArgs.toMap)  // Hypermake args are injected as environment vars
        .run
    } yield process
  }

}
