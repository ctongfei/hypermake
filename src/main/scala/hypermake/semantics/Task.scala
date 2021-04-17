package hypermake.semantics

import better.files.File
import scala.collection._
import scala.sys._
import zio._
import hypermake.core._
import hypermake.execution.RuntimeContext
import hypermake.util._


class Task(val name: Name,
           val env: Env,
           val `case`: Case,
           val inputs: Map[Name, Value],
           val inputEnvs: Map[Name, Env],
           val outputFileNames: Map[Name, Value],
           val outputEnvs: Map[Name, Env],
           val rawScript: Script)
          (implicit ctx: ParsingContext) extends Job()(ctx) {

//
//  def executeRaw: IO[Boolean] = IO {
//    val scriptFile = File(absolutePath / "script.sh")
//    scriptFile.overwrite(script.toString)
//    val command = s"${runtime.SHELL} script.sh"
//    val process = scala.sys.process.Process(command, cwd = new java.io.File(absolutePath))
//    val exitcode = process.!
//    File(absolutePath / "exitcode").overwrite(exitcode.toString)
//    exitcode == 0
//  }
//
//  def execute(semaphore: Semaphore[IO]): IO[Boolean] =
//    for {
//      _ <- semaphore.acquire
//      exitCode <- executeRaw
//      _ <- semaphore.release
//    }
//      yield exitCode

  override lazy val hashCode = id.hashCode

  override def equals(o: Any) = o match {
    case that: Task => this.id == that.id
    case _ => false
  }

  override def toString = id

}

class PointedCubeTask(val name: Name,
                      val env: Env,
                      val cases: PointedCaseCube,
                      val inputs: Map[Name, PointedCube[Value]],
                      val inputEnvs: Map[Name, Env],
                      val outputNames: Map[Name, PointedCube[Value]],
                      val outputEnvs: Map[Name, Env],
                      val script: PointedCube[Script]
                     )(implicit ctx: ParsingContext)
  extends PointedCube[Task]
{ self =>

  def get(c: Case): Option[Task] = {
    if (cases containsCase c) {
      val cc = cases.normalizeCase(c)
      val is = inputs.mapValuesE(_.select(c).default)
      val os = outputNames.mapValuesE(_.select(c).default)
      val scr = script.select(c).default
      Some(new Task(name, env, cc, is, inputEnvs, os, outputEnvs, scr))
    } else None
  }
}
