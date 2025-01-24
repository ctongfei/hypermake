package hypermake.core

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.collection._
import scala.collection.decorators._
import scala.math._

import zio._

import hypermake.cli.CLI
import hypermake.collection._
import hypermake.execution._
import hypermake.semantics.Context
import hypermake.util.Escaper._
import hypermake.util._

/**
 * A job is the atomic unit of scripts that is executed by HyperMake.
 * A job can either be a task, a package.
 */
abstract class Job(implicit ctx: Context) {

  import ctx._
  implicit val runtime: RuntimeConfig = ctx.runtime

  def name: String

  def ephemeral: Boolean

  def fileSys: FileSys

  def `case`: Case

  def inputs: Args[Value] // may contain both arguments and input files

  /**
   * Specifies the file systems that the inputs should be on before starting the job.
   * These input files may be on different file systems as specified in `inputs`.
   */
  def inputFs: Map[String, FileSys]

  def outputs: Args[Value.Output]

  def decorators: Seq[Decorator]

  lazy val services: Seq[Service] = {
    val servicesReferredByInputs = inputs.values.collect { case Value.Output(_, _, _, Some(service)) => service }
    val fileSystems = (inputFs.values ++ outputs.values.map(_.fileSys) ++ Seq(fileSys)).toSet
    val servicesFromFileSys = fileSystems
      .collect { case fs: FileSys.Custom if fs.name != "" => fs.asService }
      .collect { case Some(service) => service }
    (servicesReferredByInputs ++ servicesFromFileSys).toSeq
  }

  def rawScript: Script

  /** HyperMake environment variables provided to each job. */
  lazy val jobEnvVars = Map(
    "HYPERMAKE_JOB_ID" -> id,
    "HYPERMAKE_JOB_DISPLAY_ID" -> {
      val caseString = ctx.caseString(`case`)
      if (caseString.isEmpty) name else s"$name[$caseString]"
    },
    "HYPERMAKE_JOB_NAME" -> name,
    "HYPERMAKE_JOB_CASE" -> caseString,
    "HYPERMAKE_JOB_CASE_JSON" -> caseInJson,
    "HYPERMAKE_JOB_WD" -> path
  )

  /** Path relative to the output root of the job file system. */
  lazy val path =
    s"${name.replace('.', fileSys./)}${fileSys./}$potentiallyHashedPercentEncodedCaseString"

  lazy val absolutePath = fileSys.resolvePath(path)

  /**
   * The canonical string identifier for this task, in the percent-encoded URL format.
   * Potentially this serves as the entry point in a web server.
   */
  lazy val id = {
    val taskStr = s"${name.replace('.', '/')}"
    val argsStr = percentEncodedCaseStringUrl
    if (argsStr.isEmpty) taskStr else s"$taskStr?$argsStr"
  }

  lazy val url = s"hypermake:${fileSys}/$id"

  /** Set of dependent jobs that this job depends on. */
  lazy val dependentJobs: Set[Job] =
    (inputs ++ decorators.flatMap(_.script.args)).values.flatMap(_.dependencies).toSet

  lazy val script: Script = Script(rawScript.script, rawScript.args ++ inputs ++ outputs)

  lazy val outputAbsolutePaths =
    outputs.keySet.makeMap { x => s"$path${fileSys./}${outputs(x).value}" }

  /** The exit code of this job. If the job is not done, returns a failed effect. */
  def exitCode(implicit std: StdSinks): HIO[Int] = fileSys.read(s"$path${fileSys./}exitcode").mapEffect(_.toInt)

  /** Checks if this job is complete, i.e. job itself properly terminated and all its outputs existed. */
  def checkStatus(implicit std: StdSinks): HIO[Status] = {
    val check = for {
      exitCode <- fileSys
        .read(s"$path${fileSys./}exitcode")
        .mapEffect(_.toInt)
        .catchAll(_ => IO.succeed(-1))
      outputStatus <- checkOutputs
    } yield if (exitCode == 0) outputStatus else Status.Failure.NonZeroExitCode(exitCode)
    for {
      attempted <- fileSys.exists(path)
      status <- if (attempted) check else ZIO.succeed(Status.Pending)
    } yield status
  }

  /** An operation that prepares output of dependent jobs to the file system of this job. */
  def prepareInputs(implicit std: StdSinks): HIO[Map[String, String]] = ZIO.collectAll {
    val decoratorInputs = decorators.flatMap(_.script.args).map { case (k, v) => k -> (v, fileSys) }
    for ((name, (input, fs)) <- (inputs.args zipByKey inputFs) ++ decoratorInputs)
      yield fs
        .prepareInput(name, input, path)
        .map {
          case Some(preparedPath) => name -> Some(preparedPath) // prepared input
          case None               => name -> None // pure input, is not linked
        }
  } map { effs =>
    effs.collect({ case (k, Some(v)) => k -> v }).toMap
  }

  /** An operation that checks the output of this job and the exit status of this job. */
  def checkOutputs(implicit std: StdSinks): HIO[Status.Result] = {
    val outputStatuses = ZIO
      .collectAll {
        for ((outputName, (outputPath, outputFs)) <- outputAbsolutePaths zipByKey outputs.mapValuesE(_.fileSys))
          yield outputFs.exists(outputPath).map((outputName, _))
      }
    val missingOutputs = outputStatuses.map(_.filterNot(_._2).map(_._1).toSet)
    missingOutputs
      .map { missing =>
        if (missing.isEmpty) Status.Success else Status.Failure.MissingOutputs(missing)
      }
      .catchAll(_ => ZIO.succeed(Status.Failure.FileSysError))
  }

  /** Writes the script and decorating calls to the working directory. */
  def writeScript(
      linkedArgs: Map[String, String]
  )(implicit std: StdSinks): HIO[Map[String, String]] = {
    for {
      finalScript <- ZIO.foldLeft(decorators)(script) { case (scr, dec) =>
        fileSys
          .write(f"$path${fileSys./}script.${scr.nestingLevel}", scr.script)
          .as(dec(scr, fileSys))
      } // wraps the script with decorator calls sequentially
      _ <- fileSys.write(f"$path${fileSys./}script", finalScript.script)

      // Linked args are of the highest precedence since they are resolved from envs
      jobArgs = finalScript.args.toStrMap ++ linkedArgs
      _ <- fileSys.write(
        f"$path${fileSys./}args",
        (jobArgs.toArray.sortBy(_._1) ++ jobEnvVars.toArray)
          .map { case (k, v) => s"""$k=${Shell.escape(v)}""" }
          .mkString("", "\n", "\n")
      )
    } yield jobArgs ++ jobEnvVars
  }

  def execute(cli: CLI.Service)(implicit std: StdSinks): HIO[Status.Result] = {
    val effect = for {
      _ <- removeOutputs.ignore // may fail, but we don't care, proceed
      _ <- fileSys.mkdir(path)
      _ <- cli.update(this, Status.Waiting)
      _ <- fileSys.lock(path)
      preparedArgs <- prepareInputs
      args <- writeScript(preparedArgs)
      _ <- cli.update(this, Status.Running)
      exitCode <- fileSys.execute(path, runtime.shell, Seq("script"), args)
      outputStatus <- checkOutputs
    } yield {
      if (exitCode.code != 0) Status.Failure.NonZeroExitCode(exitCode.code)
      else outputStatus
    }
    effect.ensuring(fileSys.unlock(path).orElseSucceed(()))
  }

  def executeIfNotDone(cli: CLI.Service): HIO[(Boolean, Status.Result)] = {
    implicit val std: StdSinks = cli.sinks(this)
    for {
      status <- checkStatus
      (hasRun, successful) <-
        if (status.isSuccess) ZIO.succeed((false, Status.Success))
        else execute(cli).map((true, _))
    } yield (hasRun, successful)
  }

  def markAsDone(cli: CLI.Service): HIO[Boolean] = {
    implicit val std: StdSinks = cli.globalSinks
    val effect = for {
      _ <- fileSys.mkdir(path)
      _ <- cli.update(this, Status.Waiting)
      _ <- fileSys.lock(path)
      _ <- cli.update(this, Status.Running)
      _ <- ZIO.foreach_(outputs) { case (_, o) => o.fileSys.touch(o.value) } // pretends the outputs exist
      _ <- fileSys.write(f"$path${fileSys./}exitcode", "0")
      outputStatus <- checkOutputs
    } yield outputStatus.isSuccess
    effect.ensuring(fileSys.unlock(path).orElseSucceed(()))
  }

  def printStatus(cli: CLI.Service): HIO[Unit] = {
    implicit val std: StdSinks = cli.globalSinks
    for {
      status <- checkStatus
      _ <- cli.update(this, if (status.isSuccess) Status.Complete else status)
    } yield ()
  }

  def statusString(cli: CLI.Service): HIO[String] = {
    implicit val std: StdSinks = cli.globalSinks
    for {
      status <- checkStatus
      r <- cli.showInGraph(this, if (status.isSuccess) Status.Complete else status)
    } yield r
  }

  def isLocked(implicit std: StdSinks): HIO[Boolean] =
    fileSys.isLocked(path)

  def forceUnlock(implicit std: StdSinks): HIO[Unit] =
    fileSys.forceUnlock(path)

  def removeOutputs(implicit std: StdSinks): HIO[Unit] =
    fileSys.remove(path)

  def invalidate(implicit std: StdSinks): HIO[Unit] =
    fileSys.remove(f"$path${fileSys./}exitcode")

  def canonicalCase = ctx.canonicalizeCase(`case`)

  def percentEncodedCaseStringPath = ctx.percentEncodedCaseStringPath(`case`)
  def percentEncodedCaseStringUrl = ctx.percentEncodedCaseStringUrl(`case`)

  def potentiallyHashedPercentEncodedCaseString = {
    val s = percentEncodedCaseStringPath
    val bytes = s.getBytes(StandardCharsets.UTF_8)
    if (bytes.length > 255)
      MessageDigest.getInstance("MD5").digest(bytes).map("%02x".format(_)).mkString
    else s
  }

  def caseString = ctx.caseString(`case`)

  def caseInJson = ctx.caseInJson(`case`)

  def colorfulString = {
    import fansi._
    s"${Bold.On(Color.LightBlue(name)).render}[${colorfulCaseString(`case`)}]"
  }

  override def toString = id

  override lazy val hashCode = id.hashCode

  override def equals(o: Any) = o match {
    case that: Job => this.id == that.id
    case _         => false
  }

}

object Job {
  implicit val ordering: Ordering[Job] = Ordering.by(_.id)
}
