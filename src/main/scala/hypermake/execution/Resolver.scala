package hypermake.execution

import scala.collection._

import hypermake.core._
import hypermake.semantics._

object Resolver {

  /**
   * Performs a traversal to resolve all dependent tasks of the given targets.
   * @param targets A collection of target tasks
   * @return The task dependency DAG
   */
  def resolve(targets: Iterable[Task]): Graph[Task] = {
    val g = Graph[Task]()
    val q = mutable.Queue.from(targets)

    while (q.nonEmpty) {
      val c = q.dequeue()
      if (!g.containsNode(c)) {
        g.addNode(c)
        for (d <- c.dependentTasks) {
          g.addNode(d)
          g.addArc(d, c)
          q.enqueue(d)
        }
      }
    }
    g
  }

  /**
   * Performs a traversal to resolve all dependent tasks of the given task cubes.
   * @param targets A collection of target task cubes
   * @return The task dependency DAG
   */
  def resolveCube(targets: Iterable[Cube[Task]]) = {
    val targetTasks = targets.flatMap(_.allElements)
    resolve(targetTasks)
  }

  /**
   * Performs a traversal to resolve all dependent tasks of a given plan.
   * @param plan The given plan
   * @return The task dependency DAG
   */
  def resolvePlan(plan: Plan): Graph[Task] =
    resolveCube(plan.tasks)

}
