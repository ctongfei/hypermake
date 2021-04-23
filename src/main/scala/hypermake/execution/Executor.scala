package hypermake.execution

import scala.collection._
import cats.implicits._
import zio._
import zio.blocking._
import hypermake.core._
import hypermake.exception.JobFailedException
import hypermake.semantics._
import hypermake.util._


object Executor {

  def run(jobs: Graph[Job])(implicit runtime: RuntimeContext): HIO[Unit] = {
    val sortedJobs = jobs.topologicalSort  // may throw CyclicWorkflowException
    for {
      semaphore <- Semaphore.make(runtime.numParallelJobs)
      promises <- ZIO.foldLeft(sortedJobs)(immutable.Map[String, Promise[Throwable, Unit]]()) { (m, j) =>
        Promise.make[Throwable, Unit] map { p => m + (j.id -> p) }
      }
      effects = sortedJobs map { j =>
        for {
          _ <- ZIO.foreach_(jobs.incomingNodes(j))(i => promises(i.id).await)
          exitCode <- semaphore.withPermit(j.execute).map(_.code)
          _ <-
            if (exitCode == 0) promises(j.id).succeed(())
            else promises(j.id).fail(JobFailedException(j, exitCode))
        } yield ()
      }
      allFibers <- ZIO.forkAll(effects)
      _ <- allFibers.join
    } yield ()
  }

}
