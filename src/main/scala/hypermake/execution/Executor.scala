package hypermake.execution

import java.time._
import java.time.format._
import scala.collection._

import zio._

import hypermake.cli._
import hypermake.collection._
import hypermake.core._
import hypermake.exception.JobFailedException
import hypermake.semantics.Context
import hypermake.util._

object Executor {

  private val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss")

  def recordJobsRun(jobs: Iterable[Job], cli: CLI.Service)(implicit ctx: Context): HIO[Unit] = {
    implicit val std: StdSinks = cli.globalSinks
    val nowStr =
      dateTimeFormatter.format(Instant.now.atZone(ZoneId.systemDefault()).toLocalDateTime)
    val fs = ctx.local
    import fs._
    val logPath = s"$root${/}.runs${/}$nowStr"
    for {
      _ <- mkdir(logPath)
      u <- write(s"$logPath${/}jobs", jobs.map(_.toString).mkString("\n"))
    } yield u
  }

  def run(
      jobs: Iterable[Job]
  )(action: Job => HIO[Boolean])(implicit runtime: RuntimeConfig): HIO[Unit] = {
    for {
      semaphore <- Semaphore.make(runtime.numParallelJobs)
      u <- ZIO.foreach_(jobs) { j => semaphore.withPermit(action(j)).orElseSucceed(()) }
    } yield u
  }

  /** Runs an action over all jobs specified in the given acyclic directed graph. */
  def runDAG(jobs: Graph[Job], cli: CLI.Service)(implicit runtime: RuntimeConfig): HIO[Unit] = {
    val sortedJobs = jobs.topologicalSort.toIndexedSeq // may throw CyclicWorkflowException
    val emptyMap = immutable.Map[Job, Promise[Throwable, Unit]]()
    for {
      semaphore <- Semaphore.make(runtime.numParallelJobs)
      promises: Map[Job, Promise[Throwable, Unit]] <- ZIO.foldLeft(sortedJobs)(emptyMap) { (m, j) =>
        Promise.make[Throwable, Unit] map { p => m + (j -> p) }
      }
      effects = sortedJobs map { j =>
        for {
          _ <- ZIO.foreach_(jobs.incomingNodes(j))(i => promises(i).await)
          (hasRun, successful) <- semaphore.withPermit(j.executeIfNotDone(cli))
          u <-
            if (!hasRun)
              promises(j).succeed(()) // do not print anything to the CLI
            else if (successful)
              cli.update(j, Status.Succeeded) *> promises(j).succeed(())
            else
              cli.update(j, Status.Failed) *> (
                if (runtime.keepGoing)
                  promises(j).succeed(()) // if keep-going, ignore failure and carry on
                else promises(j).fail(JobFailedException(j))
              )
        } yield u
      }
      allFibers <- ZIO.forkAll(effects)
      u <- allFibers.join
    } yield u
  }

}
