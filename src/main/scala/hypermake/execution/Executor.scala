package hypermake.execution

import scala.collection._
import cats.implicits._
import zio._
import zio.blocking._
import hypermake.core._
import hypermake.exception.JobFailedException
import hypermake.semantics._
import hypermake.util._


class Executor(jobs: Graph[Job]) {

  def run(implicit runtime: RuntimeContext): HIO[Unit] = {
    val sortedJobs = jobs.topologicalSort
    for {
      semaphore <- Semaphore.make(runtime.numParallelJobs)
      promises <- ZIO.foldLeft(sortedJobs)(immutable.Map[String, Promise[Throwable, Unit]]()) { (m, j) =>
        Promise.make[Throwable, Unit] map { p => m + (j.id -> p) }
      }
      effects = sortedJobs map { j =>
        for {
          _ <- ZIO.foreach_(j.dependentJobs)(i => promises(i.id).await)
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

//
//  for {
//    queue: Queue[Job] <- Queue.bounded[Job](10)
//    fibers = jobs.map(j => queue.offer(j).fork)
//
//  }
//
//  private def run0(semaphore: Semaphore): HIO[Boolean] = {
//    val jobMap = jobs.map(j => j.id -> j).toMap
//    val jobIOMap = jobMap.mapValuesE(j => semaphore.withPermit(j.execute))
//    val jobExitCodeMap = jobMap.mapValuesE(j => j.isComplete)
//    val jobsWithDependencies = jobs.map { j =>
//      val dependencyExitCodes = jobMap(j.id).dependentJobs.toSet.map((j: Job) => jobExitCodeMap(j.id))
//      val prerequisitesSatisfied = ZIO.collectAll(dependencyExitCodes).map(_.forall(identity))
//      prerequisitesSatisfied *> jobIOMap(j.id)
//    }
//    ZIO.collectAllPar(jobsWithDependencies.toList).map(_.forall(_.code == 0))
//  }
//
//  def run(numParallelJobs: Int): HIO[Boolean] = for {
//    semaphore <- Semaphore.make(numParallelJobs)
//    r <- run0(semaphore)
//  } yield r

}