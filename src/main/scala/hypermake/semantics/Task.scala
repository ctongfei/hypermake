package hypermake.semantics

import better.files.File
import cats.effect.IO
import cats.effect.std.Semaphore

import scala.collection._
import scala.sys._
import hypermake.core._
import hypermake.util._


class Task(val name: Name,
           val env: Name,
           val `case`: Case,
           val inputs: Map[Name, Value],
           val inputEnvs: Map[Name, Name],
           val outputFileNames: Map[Name, Value],
           val outputEnvs: Map[Name, Name],
           val rawScript: Script)
          (implicit ctx: ParsingContext) {

  import ctx._
  implicit val runtime: RuntimeContext = ctx.runtime

  /**
   * Path to store the output of this task, relative to the output root.
   */
  lazy val path = s"$name/${escapedArgsString(`case`.underlying)}"

  /** The canonical string identifier for this task. */
  lazy val id = s"$name[${argsString(`case`.underlying)}]"

  lazy val dependentTasks: Set[Task] =
    inputs.values.map(_.dependencies).fold(Set())(_ union _)

  lazy val script: Script = rawScript.withNewArgs(inputs ++ outputFileNames)

  lazy val outputs: Map[Name, FileValue] = outputFileNames.map { case (o, s) =>
    o -> Value.TaskOutput(path / s.value, outputEnvs(o), this)
  }

  def selectOutput(output: Name): FileValue = {
    Value.TaskOutput(path / outputFileNames(output).value, outputEnvs(output), this)
  }

  def absolutePath = ctx.runtime.localOutputRoot / path

  lazy val inputAbsolutePaths: Map[Name, File] =
    inputs.mapValuesE(s => File(ctx.runtime.localOutputRoot / s.value))

  lazy val outputAbsolutePaths: Map[Name, File] =
    outputFileNames.mapValuesE(s => File(ctx.runtime.localOutputRoot / s.value))

  def isComplete: Boolean = {
    val exitCodeFile = File(absolutePath / "exitcode")
    val properlyExited = exitCodeFile.exists && (try {
      val exitCodeLine = exitCodeFile.lines.head
      exitCodeLine.toInt == 0
    } catch { case _: Throwable => false })
    val inputsSatisfied = inputAbsolutePaths.values.forall(_.exists)
    properlyExited && inputsSatisfied
  }

  def executeRaw: IO[Boolean] = IO {
    val scriptFile = File(absolutePath / "script.sh")
    scriptFile.overwrite(script.toString)
    val command = s"${runtime.SHELL} script.sh"
    val process = scala.sys.process.Process(command, cwd = new java.io.File(absolutePath))
    val exitcode = process.!
    File(absolutePath / "exitcode").overwrite(exitcode.toString)
    exitcode == 0
  }

  def execute(semaphore: Semaphore[IO]): IO[Boolean] =
    for {
      _ <- semaphore.acquire
      exitCode <- executeRaw
      _ <- semaphore.release
    }
      yield exitCode

  override lazy val hashCode = id.hashCode

  override def equals(o: Any) = o match {
    case that: Task => this.id == that.id
    case _ => false
  }

  override def toString = id

}

class PointedCubeTask(val name: Name,
                      val env: Name,
                      val cases: PointedCaseCube,
                      val inputs: Map[Name, PointedCube[Value]],
                      val inputEnvs: Map[Name, Name],
                      val outputs: Map[Name, PointedCube[Value]],
                      val outputEnvs: Map[Name, Name],
                      val script: PointedCube[Script]
                     )(implicit ctx: ParsingContext)
  extends PointedCube[Task]
{ self =>

  def get(c: Case): Option[Task] = {
    if (cases containsCase c) {
      val cc = cases.normalizeCase(c)
      val is = inputs.mapValuesE(_.select(c).default)
      val os = outputs.mapValuesE(_.select(c).default)
      val scr = script.select(c).default
      Some(new Task(name, env, cc, is, inputEnvs, os, outputEnvs, scr))
    } else None
  }
}
