package hypermake.core

import hypermake.cli.CLI
import hypermake.collection._
import hypermake.execution._
import hypermake.semantics.Context
import hypermake.util.Escaper._
import hypermake.util._
import zio._

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.collection._
import scala.collection.decorators._

/** A job is any block of shell script that is executed by HyperMake. A job can either be a task, a package, or a
  * service.
  *
  * @param ctx
  *   Parsing context that yielded this job
  */
abstract class Job(implicit ctx: Context) {

  import ctx._

  implicit val runtime: RuntimeConfig = ctx.runtime

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

  def decorators: Seq[Call]

  def rawScript: Script

  /** Global variables included for this job. */
  lazy val globalArgs = ctx.globalValues.map { case (k, v) => k.name -> v.select(`case`).default.value }

  lazy val jobCaseArgs = Map(
    "HYPERMAKE_JOB_ID" -> id,
    "HYPERMAKE_JOB_NAME" -> name.name,
    "HYPERMAKE_JOB_CASE" -> caseInJson
  )

  /** Path to store the output of this task, relative to the output root. This is the working directory of this task if
    * executed.
    */
  lazy val path = s"${name.name.replace(".", "/")}/$potentiallyHashedPercentEncodedCaseString"

  lazy val absolutePath = env.resolvePath(path)

  /** The canonical string identifier for this task, in the percent-encoded URL format. */
  lazy val id = s"${name.name.replace(".", "/")}?$percentEncodedCaseString"

  /** Set of dependent jobs. */
  lazy val dependentJobs: Set[Job] =
    (inputs ++ decorators.map(_.args).fold(Iterable())(_ ++ _)).values.flatMap(_.dependencies).toSet

  lazy val script: Script = Script(rawScript.script, rawScript.args ++ inputs, rawScript.outputArgs ++ outputs)(runtime)

  lazy val outputAbsolutePaths =
    outputs.keySet.makeMap { x => env.resolvePath(outputs(x).value, absolutePath) }

  /** Checks if this job is complete, i.e. job itself properly terminated and all its outputs existed. */
  def isDone(implicit std: StdSinks): HIO[Boolean] = for {
    exitCode <- env.read(env.resolvePath("exitcode", absolutePath)).map(_.toInt).catchAll(_ => IO.succeed(-1))
    outputsExist <- checkOutputs
  } yield exitCode == 0 && outputsExist

  /** An operation that links output of dependent jobs to the working directory of this job. */
  def linkInputs(implicit std: StdSinks): HIO[Map[String, String]] = ZIO.collectAll {
    for ((Name(name), (input, inputEnv)) <- inputs zipByKey inputEnvs)
      yield inputEnv.linkValue(input, inputEnv.resolvePath(name, absolutePath)).as(name -> Some(name))
  } map { effs =>
    effs.collect({ case (k, Some(v)) => k -> v }).toMap
  }

  /** An operation that checks the output of this job and the exit status of this job. */
  def checkOutputs(implicit std: StdSinks): HIO[Boolean] = {
    ZIO
      .collectAll {
        for ((_, (outputPath, outputEnv)) <- outputAbsolutePaths zipByKey outputEnvs)
          yield outputEnv.exists(outputPath)
      }
      .map(_.forall(identity))
      .catchAll(_ => IO.succeed(false))
  }

  /** Writes the script and decorating calls to the working directory. */
  def writeScript(linkedArgs: Map[String, String])(implicit std: StdSinks): HIO[Map[String, String]] = {
    val scriptNames = decorators.map(_.inputScriptFilename)
    for {
      finalScript <- ZIO.foldLeft(decorators zip scriptNames)(script) { case (scr, (c, name)) =>
        env.write(absolutePath / name, scr.script) as c(scr)
      } // wraps the script with decorator calls sequentially
      _ <- env.write(absolutePath / "script.sh", finalScript.script)

      // Linked args are of the highest precedence since they are resolved from envs
      mergedArgs = jobCaseArgs ++ globalArgs ++ finalScript.strArgs ++ linkedArgs
      _ <- env.write(
        absolutePath / "args",
        mergedArgs
          .map { case (k, v) => s"""$k=${Shell.escape(v)}""" }
          .mkString("", "\n", "\n")
      )
    } yield mergedArgs
  }

  def execute(cli: CLI.Service)(implicit std: StdSinks): HIO[Boolean] = {
    val effect = for {
      _ <- removeOutputs
      _ <- env.mkdir(absolutePath)
      _ <- cli.update(this, Status.Waiting)
      _ <- env.lock(absolutePath)
      linkedArgs <- linkInputs
      args <- writeScript(linkedArgs)
      _ <- cli.update(this, Status.Running)
      exitCode <- env.execute(absolutePath, runtime.shell, Seq("script.sh"), args)
      hasOutputs <- checkOutputs
    } yield (exitCode.code == 0) && hasOutputs
    effect.ensuring(env.unlock(absolutePath).orElseSucceed())
  }

  def executeIfNotDone(cli: CLI.Service): HIO[(Boolean, Boolean)] = {
    implicit val std: StdSinks = cli.sinks(this)
    for {
      done <- isDone
      (hasRun, successful) <-
        if (done) ZIO.succeed((false, true))
        else execute(cli).map((true, _))
    } yield (hasRun, successful)
  }

  def markAsDone(cli: CLI.Service): HIO[Boolean] = {
    implicit val std: StdSinks = cli.globalSinks
    val effect = for {
      _ <- env.mkdir(absolutePath)
      _ <- cli.update(this, Status.Waiting)
      _ <- env.lock(absolutePath)
      _ <- cli.update(this, Status.Running)
      _ <- ZIO.foreach_(outputAbsolutePaths zipByKey outputEnvs) { case (_, (p, e)) => e.touch(p) }
      _ <- env.write(absolutePath / "exitcode", "0")
      hasOutputs <- checkOutputs
    } yield hasOutputs
    effect.ensuring(env.unlock(absolutePath).orElseSucceed())
  }

  def printStatus(cli: CLI.Service): HIO[Unit] = {
    implicit val std: StdSinks = cli.globalSinks
    for {
      done <- isDone
      _ <- cli.update(this, if (done) Status.Complete else Status.Pending)
    } yield ()
  }

  def statusString(cli: CLI.Service): HIO[String] = {
    implicit val std: StdSinks = cli.globalSinks
    for {
      done <- isDone
      r <- cli.showInGraph(this, if (done) Status.Complete else Status.Pending)
    } yield r
  }

  def isLocked(implicit std: StdSinks): HIO[Boolean] =
    env.isLocked(absolutePath)

  def forceUnlock(implicit std: StdSinks): HIO[Unit] =
    env.forceUnlock(absolutePath)

  def removeOutputs(implicit std: StdSinks): HIO[Unit] =
    env.delete(absolutePath)

  def invalidate(implicit std: StdSinks): HIO[Unit] =
    env.delete(absolutePath / "exitcode")

  def canonicalCase = ctx.canonicalizeCase(`case`)

  def percentEncodedCaseString = ctx.percentEncodedCaseString(`case`)

  def potentiallyHashedPercentEncodedCaseString = {
    val s = percentEncodedCaseString
    val bytes = s.getBytes(StandardCharsets.UTF_8)
    if (bytes.length > 255)
      MessageDigest.getInstance("MD5").digest(bytes).map("%02x".format(_)).mkString
    else s
  }

  def caseString = ctx.caseString(`case`)

  def caseInJson = ctx.caseInJson(`case`)

  def colorfulString = {
    import fansi._
    s"${Bold.On(Color.LightBlue(name.name)).render}[${colorfulCaseString(`case`)}]"
  }

  override def toString = id

  override lazy val hashCode = id.hashCode

  override def equals(o: Any) = o match {
    case that: Job => this.id == that.id
    case _         => false
  }

}
