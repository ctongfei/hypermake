package hypermake.execution

import scala.collection._
import zio._
import hypermake.collection._
import hypermake.exception.JobFailedException
import hypermake.cli._
import hypermake.core._
import hypermake.semantics.SymbolTable
import hypermake.util._
import java.time._
import java.time.format._


object Executor {

  private val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss")

  def backupJob(jobs: Iterable[Job])(implicit ctx: SymbolTable): HIO[Unit] = {
    val nowStr = dateTimeFormatter.format(Instant.now.atZone(ZoneId.systemDefault()).toLocalDateTime)
    val env = ctx.localEnv
    val logPath = s"${env.root}/.runs/$nowStr"
    for {
      _ <- env.mkdir(logPath)
      u <- env.write(s"$logPath/jobs", jobs.map(_.toString).mkString("\n"))
    } yield u
  }

  def run(jobs: Iterable[Job])(action: Job => HIO[Boolean])(implicit runtime: RuntimeContext): HIO[Unit] = {
    for {
      semaphore <- Semaphore.make(runtime.numParallelJobs)
      u <- ZIO.foreach_(jobs) { j => semaphore.withPermit(action(j)).orElseSucceed(()) }
    } yield u
  }

  /**
   * Runs an action over all jobs specified in the given acyclic directed graph.
   */
  def runDAG(jobs: Graph[Job], cli: CLI.Service)(implicit runtime: RuntimeContext): HIO[Unit] = {
    val sortedJobs = jobs.topologicalSort.toIndexedSeq  // may throw CyclicWorkflowException
    for {
      semaphore <- Semaphore.make(runtime.numParallelJobs)
      promises: Map[Job, Promise[Throwable, Unit]] <- ZIO.foldLeft(sortedJobs)(immutable.Map[Job, Promise[Throwable, Unit]]()) { (m, j) =>
        Promise.make[Throwable, Unit] map { p => m + (j -> p) }
      }
      effects = sortedJobs map { j =>
        for {
          _ <- ZIO.foreach_(jobs.incomingNodes(j))(i => promises(i).await)
          (hasRun, successful) <- semaphore.withPermit(j.executeIfNotDone(cli))
          u <-
            if (!hasRun)
              cli.update(j, Status.Complete) *> promises(j).succeed(())
            else if (successful)
              cli.update(j, Status.Succeeded) *> promises(j).succeed(())
            else cli.update(j, Status.Failed) *> promises(j).fail(JobFailedException(j))
        } yield u
      }
      allFibers <- ZIO.forkAll(effects)
      u <- allFibers.join
    } yield u
  }

}
