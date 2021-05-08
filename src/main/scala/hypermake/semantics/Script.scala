package hypermake.semantics

import java.io.{File => JFile}
import scala.collection._
import better.files._
import zio._
import zio.process._
import hypermake.core._
import hypermake.execution._
import hypermake.util._


/**
 * Encapsulates a script together with its external arguments.
 */
case class Script(
                   script: String,
                   interpreter: String,
                   suffix: String,
                   args: Map[Name, Value]
                 )(implicit runtime: RuntimeContext) {

  def withNewArgs(newArgs: Map[Name, Value]) = Script(script, interpreter, suffix, args ++ newArgs)

  def withArgs(newArgs: (String, String)*) =
    withNewArgs(newArgs.map { case (a, v) => Name(a) -> Value.Pure(v) }.toMap)

  def fileName: String = s"script.$suffix"

  def strArgs: Map[String, String] = args.map { case (k, v) => k.name -> v.value }

  /**
   * Writes this script as a local temporary file and executes it with its arguments.
   * @param workDir The working directory to run this temporary script.
   *                By default this is the working directory of the outer Hypermake process.
   */
  def executeLocally(workDir: String = runtime.workDir): HIO[Process] = {
    val tempScriptFile = runtime.tempFile(suffix = s".$suffix")
    // allows the case where the interpreter has arguments: python -v ...
    val command = s"$interpreter $tempScriptFile".split(' ')

    for {
      _ <- IO { File(tempScriptFile).write(this.toString) }
      process <- Command(command.head, command.tail: _*)
        .workingDirectory(new JFile(workDir))
        .env(strArgs.toMap)  // Hypermake args are injected as environment vars
        .run
    } yield process
  }

}

object Script {

  def apply(script: String, suffix: String = "sh", args: Map[Name, Value] = Map())
           (implicit runtime: RuntimeContext): Script =
    Script(script, runtime.SHELL, suffix, args)

}
