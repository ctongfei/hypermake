package hypermake.execution

import scala.collection._
import cats.implicits._
import zio._
import zio.blocking._
import hypermake.semantics._
import hypermake.util._


class Executor(jobs: Seq[Job]) {

  private def run0(semaphore: Semaphore): ZIO[Blocking, Throwable, Boolean] = {
    val jobMap = jobs.map(j => j.id -> j).toMap
    val jobIOMap = jobMap.mapValuesE(j => semaphore.withPermit(j.execute))
    val jobsWithDependencies = jobs.map { j =>
      val dependencyExitCodes = jobMap(j.id).dependentJobs.toSet.map((j: Job) => jobIOMap(j.id))
      val prerequisitesSatisfied = ZIO.collectAll(dependencyExitCodes).map(_.forall(_.code == 0))
      prerequisitesSatisfied *> jobIOMap(j.id)
    }
    ZIO.collectAllPar(jobsWithDependencies.toList).map(_.forall(_.code == 0))
  }

  def run(numParallelJobs: Int) = for {
    semaphore <- Semaphore.make(numParallelJobs)
    r <- run0(semaphore)
  } yield r

}
