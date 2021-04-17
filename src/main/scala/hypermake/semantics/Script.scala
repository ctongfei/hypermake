package hypermake.semantics

import scala.collection._
import hypermake.core._
import hypermake.execution.RuntimeContext
import hypermake.util.Escaper._
import zio._
import zio.blocking.Blocking
import zio.process._
import better.files._

import java.util.UUID


case class Script(script: String, suffix: String = "sh", args: Map[Name, Value] = Map()) {

  def withNewArgs(newArgs: Map[Name, Value]) = Script(script, suffix, args ++ newArgs)

  def withArgs(newArgs: (String, String)*) =
    withNewArgs(newArgs.map { case (a, v) => Name(a) -> Value.Pure(v) }.toMap)

  def argsAssignmentScript =
    args.view.map { case (k, v) => s"""export $k="${C.escape(v.value)}""""}.mkString("\n")

  override def toString =
    f"$argsAssignmentScript\n\n$script"

  def fileName: String = s"script.$suffix"

  def executeUnmanaged(wd: String)(implicit runtime: RuntimeContext): ZIO[Blocking, Throwable, Process] = {
    val tempScriptFile = runtime.tempFile(suffix = s".$suffix")
    for {
      _ <- IO { File(tempScriptFile).write(this.toString) }
      process <- Command(runtime.SHELL, tempScriptFile)
        .workingDirectory(new java.io.File(wd)).run
    } yield process
  }

  def executeUnmanaged()(implicit runtime: RuntimeContext): ZIO[Blocking, Throwable, Process] =
    executeUnmanaged(runtime.workDir)

}
