package hypermake.semantics

import better.files.File

import scala.collection._
import scala.collection.decorators._
import zio._
import hypermake.core._
import hypermake.execution._
import hypermake.util._
import zio.blocking.Blocking

/**
 * A job is any block of shell script that is executed by HyperMake.
 * A job can either be a task, a package, or a service.
 * @param ctx Parsing context that yielded this job
 */
abstract class Job(implicit ctx: ParsingContext) {

  import ctx._
  implicit def runtime: RuntimeContext = ctx.runtime

  def name: Name
  def env: Env

  def `case`: Case
  def inputs: Map[Name, Value]
  def inputEnvs: Map[Name, Env]
  def outputFileNames: Map[Name, Value]

  def outputs: Map[Name, Value.Output] = outputFileNames.map { case (k, v) =>
    k -> Value.Output(v.value, outputEnvs.getOrElse(k, env), this)
  }
  def outputEnvs: Map[Name, Env]
  def rawScript: Script

  /**
   * Path to store the output of this task, relative to the output root.
   * This is the working directory of this task if executed.
   */
  lazy val path = s"$name/${escapedArgsString(`case`.underlying)}"

  lazy val absolutePath = env.resolvePath(path)

  /** The canonical string identifier for this task. */
  lazy val id = s"$name[${argsString(`case`.underlying)}]"

  /** Set of dependent jobs. */
  lazy val dependentJobs: Set[Job] =
    inputs.values.map(_.dependencies).fold(Set())(_ union _)

  lazy val script: Script = rawScript.withNewArgs(inputs ++ outputs)

  lazy val inputAbsolutePaths =
    inputs.keySet.makeMap { x => env.resolvePath(inputs(x).value) }

  lazy val outputAbsolutePaths =
    outputs.keySet.makeMap { x => env.resolvePath(outputs(x).value, path) }

  /** Checks if this job is complete, i.e. job itself properly terminated and all its outputs existed. */
  def isComplete: ZIO[Blocking, Throwable, Boolean] = for {
    exitCode <- env.read(env.resolvePath("exitcode", absolutePath)).map(_.toInt)
    outputsExist <- checkOutputs
  } yield exitCode == 0 && outputsExist

  /** An operation that links output of dependent jobs to the working directory of this job. */
  def linkInputs: ZIO[Blocking, Throwable, Unit] = ZIO.collectAll_ {
    for ((Name(input), (inputPath, inputEnv)) <- inputAbsolutePaths zipByKey inputEnvs)
      yield if (inputEnv == env)
        env.link(inputPath, path / input)
      else env.copyFrom(inputPath, inputEnv, path / input)
  }

  /** An operation that checks the output of this job and the exit status of this job. */
  def checkOutputs: ZIO[Blocking, Throwable, Boolean] = ZIO.collectAll {
    for ((_, (outputPath, outputEnv)) <- outputAbsolutePaths zipByKey outputEnvs)
      yield outputEnv.exists(outputPath)
  }.map(_.forall(identity))

  def execute: ZIO[Blocking, Throwable, ExitCode] = {
    for {
      _ <- env.mkdir(absolutePath)
      _ <- env.write(absolutePath / script.fileName, script.toString)
      process <- env.execute(absolutePath, runtime.SHELL, Seq(script.fileName))
      exitCode <- process.exitCode
    } yield exitCode
  }

  def clearOutputs: ZIO[Blocking, Throwable, Unit] =
    env.delete(absolutePath)

  override def toString = id
  override lazy val hashCode = id.hashCode

  override def equals(o: Any) = o match {
    case that: Job => this.id == that.id
    case _ => false
  }

}
