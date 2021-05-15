package hypermake.semantics

import better.files.File

import scala.collection._
import scala.collection.decorators._
import zio._
import zio.console._
import hypermake.core._
import hypermake.execution._
import hypermake.util._

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
  lazy val id = s"$name[$argsString]"

  /** Set of dependent jobs. */
  lazy val dependentJobs: Set[Job] =
    inputs.values.map(_.dependencies).fold(Set())(_ union _)

  lazy val script: Script = rawScript.withNewArgs(inputs ++ outputs)

  lazy val outputAbsolutePaths =
    outputs.keySet.makeMap { x => env.resolvePath(outputs(x).value, absolutePath) }

  /** Checks if this job is complete, i.e. job itself properly terminated and all its outputs existed. */
  def isDone: HIO[Boolean] = for {
    exitCode <- env.read(env.resolvePath("exitcode", absolutePath)).map(_.toInt).catchAll(_ => IO.succeed(-1))
    outputsExist <- checkOutputs
  } yield exitCode == 0 && outputsExist

  /** An operation that links output of dependent jobs to the working directory of this job. */
  def linkInputs: HIO[Unit] = ZIO.collectAllPar_ {
    for ((Name(name), (input, inputEnv)) <- inputs zipByKey inputEnvs)
      yield inputEnv.linkValue(input, inputEnv.resolvePath(name, absolutePath))
  }

  /** An operation that checks the output of this job and the exit status of this job. */
  def checkOutputs: HIO[Boolean] = {
    ZIO.collectAll {
      for ((_, (outputPath, outputEnv)) <- outputAbsolutePaths zipByKey outputEnvs)
        yield outputEnv.exists(outputPath)
    }.map(_.forall(identity)).catchAll(_ => IO.succeed(false))
  }

  def execute: HIO[Boolean] = for {
    _ <- env.mkdir(absolutePath)
    _ <- env.lock(absolutePath)
    _ <- env.write(absolutePath / script.fileName, script.script)
    _ <- linkInputs
    _ <- putStrLn(f"⚙️  Running job $colorfulString...")
    exitCode <- env.execute(absolutePath, script.interpreter, Seq(script.fileName), script.strArgs)
    _ <- env.unlock(absolutePath)
  } yield exitCode.code == 0

  def executeIfNotDone: HIO[Boolean] = for {
    done <- isDone
    successful <- if (done)
      putStrLn(f"Job $colorfulString is done. Skipped.") as true
      else execute
  } yield successful

  def removeOutputs: HIO[Unit] =
    env.delete(absolutePath)

  def argsString = ctx.argsString(`case`.underlying)

  def colorfulString = {
    import fansi._
    s"${Bold.On(Color.LightBlue(name.name)).render}[${colorfulArgsString(`case`.underlying)}]"
  }

  override def toString = id
  override lazy val hashCode = id.hashCode

  override def equals(o: Any) = o match {
    case that: Job => this.id == that.id
    case _ => false
  }

}
