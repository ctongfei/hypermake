package hypermake.core

import java.io.{File => JFile}
import scala.collection._

import better.files._
import zio._
import zio.process._

import hypermake.execution._
import hypermake.util._

/** Encapsulates a script together with its external arguments (passed in as environment variables). */
case class Script(
    script: String,
    args: Args[Value] = Args(Map()),
    nestingLevel: Int = 0
) {

  def withNewArgs(newArgs: Args[Value]) = Script(script, args ++ newArgs, nestingLevel)

  def withArgs(newArgs: (String, String)*) =
    withNewArgs(Args(newArgs.toMap.mapValuesE(Value.Pure)))

  /**
   * Writes this script as a local temporary file and executes it with its arguments.
   * @param workDir The working directory to run this temporary script.
   */
  def executeLocally(
      workDir: String
  )(implicit runtime: RuntimeConfig, std: StdSinks): HIO[Process] = {
    val tempScriptFile = runtime.newTempFile(prefix = "hypermake_temp_script")
    // allows the case where the interpreter has arguments: python -v ...
    // TODO: proper handling of bash quotes
    val command = s"${runtime.shell} $tempScriptFile".split(' ').toIndexedSeq
    for {
      _ <- ZIO.attempt { File(tempScriptFile).write(script) }
      process <- Command(command.head, command.tail: _*)
        .workingDirectory(new JFile(workDir))
        .env(runtime.envVars ++ args.toStrMap) // Hypermake args are injected as environment vars;
        .stderr(ProcessOutput.Pipe)
        .stdout(ProcessOutput.Pipe)
        .run
      _ <- process.stdout.stream.run(std.out) <&> process.stderr.stream.run(std.err)
    } yield process
  }

}
