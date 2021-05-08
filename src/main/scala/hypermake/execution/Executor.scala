package hypermake.execution

import scala.collection._
import zio._
import hypermake.core._
import hypermake.exception.JobFailedException
import hypermake.semantics._
import hypermake.util._


object Executor {

  def run(jobs: Iterable[Job], action: Job => HIO[Boolean])(implicit runtime: RuntimeContext): HIO[Unit] = {
    for {
      semaphore <- Semaphore.make(runtime.numParallelJobs)
      _ <- ZIO.foreach_(jobs) { j => semaphore.withPermit(action(j)) }
    } yield ()
  }

  /**
   * Runs an action over all jobs specified in the given acyclic directed graph.
   */
  def runDAG[A](jobs: Graph[Job])(action: Job => HIO[Boolean])(implicit runtime: RuntimeContext): HIO[Unit] = {
    val sortedJobs = jobs.topologicalSort  // may throw CyclicWorkflowException
    for {
      semaphore <- Semaphore.make(runtime.numParallelJobs)
      promises <- ZIO.foldLeft(sortedJobs)(immutable.Map[String, Promise[Throwable, Unit]]()) { (m, j) =>
        Promise.make[Throwable, Unit] map { p => m + (j.id -> p) }
      }
      effects = sortedJobs map { j =>
        for {
          _ <- ZIO.foreach_(jobs.incomingNodes(j))(i => promises(i.id).await)
          succeeded <- semaphore.withPermit(action(j))
          _ <- if (succeeded) promises(j.id).succeed(())
            else promises(j.id).fail(JobFailedException(j))
        } yield ()
      }
      allFibers <- ZIO.forkAll(effects)
      _ <- allFibers.join
    } yield ()
  }

}
