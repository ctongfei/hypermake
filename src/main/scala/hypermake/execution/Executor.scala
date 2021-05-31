package hypermake.execution

import scala.collection._
import zio._
import hypermake.collection._
import hypermake.exception.JobFailedException
import hypermake.cli._
import hypermake.core._
import hypermake.util._


object Executor {

  def run(jobs: Iterable[Job])(action: Job => HIO[Boolean])(implicit runtime: RuntimeContext): HIO[Unit] = {
    for {
      semaphore <- Semaphore.make(runtime.numParallelJobs)
      _ <- ZIO.foreach_(jobs) { j => semaphore.withPermit(action(j)) }
    } yield ()
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
          _ <- if (!hasRun) cli.update(j, Status.Complete) *> promises(j).succeed(())
          else if (successful) cli.update(j, Status.Succeeded) *> promises(j).succeed(())
          else cli.update(j, Status.Failed) *> promises(j).fail(JobFailedException(j))
        } yield ()
      }
      allFibers <- ZIO.forkAll(effects)
      _ <- allFibers.join
    } yield ()
  }

}
