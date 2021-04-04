package hypermake.execution

import cats.implicits._
import cats.effect.IO
import cats.effect.implicits._
import cats.effect.std.Semaphore
import hypermake.core._
import hypermake.semantics.Task
import hypermake.util._

import scala.collection._

class Executor(tasks: Map[String, Task]) {

  def run(semaphore: Semaphore[IO]): IO[Boolean] = {
    val rawTasks = tasks.mapValuesE(_.execute(semaphore))
    val tasksWithDependencies = rawTasks.map { case (id, rawTask) =>
      val dependencyExitCodes = tasks(id).dependentTasks.toSet.map((t: Task) => rawTasks(t.id)).parUnorderedSequence
      val prerequisitesSatisfied = dependencyExitCodes.map(_.forall(x => x))
      prerequisitesSatisfied *> rawTask
    }
    tasksWithDependencies.toList.sequence.map(_.forall(x => x))
  }

}
