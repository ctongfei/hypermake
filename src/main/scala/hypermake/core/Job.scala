package hypermake.core

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.collection._
import scala.collection.decorators._

import zio._

import hypermake.cli.CLI
import hypermake.collection._
import hypermake.execution._
import hypermake.semantics.Context
import hypermake.util.Escaper._
import hypermake.util._

/**
 * A job is the atomic unit of scripts that is executed by HyperMake.
 * A job can either be a task, a package, or a service.
 */
abstract class Job(implicit ctx: Context) {

  import ctx._
  implicit val runtime: RuntimeConfig = ctx.runtime

  def name: String

  def fileSys: FileSys

  def `case`: Case

  def inputs: Map[String, Value] // may contain both arguments and input files

  /**
   * Specifies the file systems that the inputs should be on before starting the job.
   * These input files may be on different file systems as specified in `inputs`.
   */
  def inputFs: Map[String, FileSys]

  def outputs: Map[String, Value.Output]

  def decorators: Seq[Decorator]

  def rawScript: Script

  /** HyperMake environment variables provided to each job. */
  lazy val jobCaseArgs = Map(
    "HYPERMAKE_JOB_ID" -> id,
    "HYPERMAKE_JOB_NAME" -> name,
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

  /** Set of dependent jobs that this job depends on. */
  lazy val dependentJobs: Set[Job] =
    (inputs ++ decorators.flatMap(_.script.args)).values.flatMap(_.dependencies).toSet

  lazy val script: Script = Script(rawScript.script, rawScript.args ++ inputs ++ outputs)

  lazy val outputAbsolutePaths =
    outputs.keySet.makeMap { x => fileSys.resolvePath(outputs(x).value, absolutePath) }

  /** Checks if this job is complete, i.e. job itself properly terminated and all its outputs existed. */
  def isDone(implicit std: StdSinks): HIO[Boolean] = for {
    exitCode <- fileSys
      .read(fileSys.resolvePath("exitcode", absolutePath))
      .mapEffect(_.toInt)
      .catchAll(_ => IO.succeed(-1))
    outputsExist <- checkOutputs
  } yield exitCode == 0 && outputsExist

  /** An operation that links output of dependent jobs to the working directory of this job. */
  def linkInputs(implicit std: StdSinks): HIO[Map[String, String]] = ZIO.collectAll {
    val decoratorInputs = decorators.flatMap(_.script.args).map { case (k, v) => k -> (v, fileSys) }
    for ((name, (input, fs)) <- (inputs zipByKey inputFs) ++ decoratorInputs)
      yield fs
        .linkValue(input, fs.resolvePath(name, absolutePath))
        .map {
          case Some(_) => name -> Some(name) // linked input
          case None    => name -> None // pure input, is not linked
        }
  } map { effs =>
    effs.collect({ case (k, Some(v)) => k -> v }).toMap
  }

  /** An operation that checks the output of this job and the exit status of this job. */
  def checkOutputs(implicit std: StdSinks): HIO[Boolean] = {
    ZIO
      .collectAll {
        for ((_, (outputPath, outputFs)) <- outputAbsolutePaths zipByKey outputs.mapValuesE(_.fileSys))
          yield outputFs.exists(outputPath)
      }
      .map(_.forall(identity))
      .catchAll(_ => IO.succeed(false))
  }

  /** Writes the script and decorating calls to the working directory. */
  def writeScript(
      linkedArgs: Map[String, String]
  )(implicit std: StdSinks): HIO[Map[String, String]] = {
    for {
      finalScript <- ZIO.foldLeft(decorators)(script) { case (scr, dec) =>
        fileSys
          .write(f"$absolutePath${fileSys./}script.${scr.nestingLevel}", scr.script)
          .as(dec(scr, fileSys))
      } // wraps the script with decorator calls sequentially
      _ <- fileSys.write(absolutePath / "script", finalScript.script)

      // Linked args are of the highest precedence since they are resolved from envs
      jobArgs = finalScript.strArgs(runtime) ++ linkedArgs
      _ <- fileSys.write(
        absolutePath / "args",
        (jobArgs.toArray.sortBy(_._1) ++ jobCaseArgs.toArray)
          .map { case (k, v) => s"""$k=${Shell.escape(v)}""" }
          .mkString("", "\n", "\n")
      )
    } yield jobArgs ++ jobCaseArgs
  }

  def execute(cli: CLI.Service)(implicit std: StdSinks): HIO[Boolean] = {
    val effect = for {
      _ <- removeOutputs.ignore // may fail, but we don't care, proceed
      _ <- fileSys.mkdir(absolutePath)
      _ <- cli.update(this, Status.Waiting)
      _ <- fileSys.lock(absolutePath)
      linkedArgs <- linkInputs
      args <- writeScript(linkedArgs)
      _ <- cli.update(this, Status.Running)
      exitCode <- fileSys.execute(path, runtime.shell, Seq("script"), args)
      hasOutputs <- checkOutputs
    } yield (exitCode.code == 0) && hasOutputs
    val potentiallyAbsolvedEffect =
      if (runtime.keepGoing) effect.catchAll(_ => ZIO.succeed(false)) else effect
    potentiallyAbsolvedEffect.ensuring(fileSys.unlock(absolutePath).orElseSucceed())
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
      _ <- fileSys.mkdir(absolutePath)
      _ <- cli.update(this, Status.Waiting)
      _ <- fileSys.lock(absolutePath)
      _ <- cli.update(this, Status.Running)
      _ <- ZIO.foreach_(outputs) { case (_, o) => o.fileSys.touch(o.value) }
      _ <- fileSys.write(absolutePath / "exitcode", "0")
      hasOutputs <- checkOutputs
    } yield hasOutputs
    effect.ensuring(fileSys.unlock(absolutePath).orElseSucceed())
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
    fileSys.isLocked(absolutePath)

  def forceUnlock(implicit std: StdSinks): HIO[Unit] =
    fileSys.forceUnlock(absolutePath)

  def removeOutputs(implicit std: StdSinks): HIO[Unit] =
    fileSys.delete(absolutePath)

  def invalidate(implicit std: StdSinks): HIO[Unit] =
    fileSys.delete(absolutePath / "exitcode")

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
