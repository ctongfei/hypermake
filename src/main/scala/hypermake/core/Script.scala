package hypermake.core

import cats.implicits._
import cats.syntax.unorderedTraverse._
import better.files._
import cats.UnorderedTraverse
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
    nestingLevel: Int = 0
) {

  def withNewArgs(newArgs: Map[String, Value]) = Script(script, args ++ newArgs, nestingLevel)

  def withArgs(newArgs: (String, String)*) =
    withNewArgs(newArgs.toMap.mapValuesE(Value.Pure))

  def strArgs(implicit runtime: RuntimeConfig): Map[String, String] = {
    args.map { case (k, v) => k -> v.value }
  }

  /** Writes this script as a local temporary file and executes it with its arguments.
    *
    * @param workDir
    *   The working directory to run this temporary script.
    */
  def executeLocally(
      workDir: String
  )(implicit runtime: RuntimeConfig, std: StdSinks): HIO[Process] = {
    val tempScriptFile = runtime.newTempFile(prefix = "hypermake_temp_script")
    // allows the case where the interpreter has arguments: python -v ...
    // TODO: proper handling of bash quotes
    val command = s"${runtime.shell} $tempScriptFile".split(' ')
    for {
      _ <- IO {
        File(tempScriptFile).write(script)
      }
      process <- Command(command.head, command.tail: _*)
        .workingDirectory(new JFile(workDir))
        .env(runtime.envVars ++ strArgs.toMap) // Hypermake args are injected as environment vars;
        .stderr(ProcessOutput.Pipe)
        .stdout(ProcessOutput.Pipe)
        .run
      _ <- process.stdout.stream.run(std.out) <&> process.stderr.stream.run(std.err)
    } yield process
  }

}
//
//class PointedCubeScript(
//    val cases: PointedCaseTensor,
//    val script: String,
//    val args: Map[String, PointedTensor[Value]] = Map(),
//    val nestingLevel: Int = 0
//)(implicit runtime: RuntimeConfig)
//    extends PointedTensor[Script] {
//
//  def get(c: Case) = args.toMap.unorderedSequence.get(c).map(as => Script(script, as, nestingLevel))
//
//}
