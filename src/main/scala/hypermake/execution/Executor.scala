package hypermake.execution

import scala.collection._
import zio._
import hypermake.core._
import hypermake.exception.JobFailedException
import hypermake.semantics._
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
  def runDAG(jobs: Graph[Job])(implicit runtime: RuntimeContext): HIO[Unit] = {
    val sortedJobs = jobs.topologicalSort  // may throw CyclicWorkflowException
    val statusMonitor = new StatusMonitor(sortedJobs.toIndexedSeq)
    for {
      semaphore <- Semaphore.make(runtime.numParallelJobs)
      promises <- ZIO.foldLeft(sortedJobs)(immutable.Map[Job, Promise[Throwable, Unit]]()) { (m, j) =>
        Promise.make[Throwable, Unit] map { p => m + (j -> p) }
      }
      _ <- statusMonitor.initialize
      effects = sortedJobs map { j =>
        for {
          _ <- ZIO.foreach_(jobs.incomingNodes(j))(i => promises(i).await)
          succeeded <- semaphore.withPermit(j.executeIfNotDone(statusMonitor))
          _ <- if (succeeded) statusMonitor.updateStatus(j, Status.Succeeded) *> promises(j).succeed(())
            else statusMonitor.updateStatus(j, Status.Failed) *> promises(j).fail(JobFailedException(j))
        } yield ()
      }
      allFibers <- ZIO.forkAll(effects)
      _ <- allFibers.join
      _ <- statusMonitor.tearDown
    } yield ()
  }

}
