package hypermake.semantics

import hypermake.core._

import scala.collection._

/**
 * Represents a collection of slices of tasks whose execution can be invoked from the command line.
 */
class Plan(val targets: Seq[Cube[Task]]) {

  /**
   * Performs a traversal to resolve all dependent tasks of a given plan.
   * @return The task dependency DAG
   */
  def dependencyGraph: Graph[Job] = {
    val targetJobs = targets.flatMap(_.allElements)
    Graph.resolveFromTargets(targetJobs, _.dependentJobs)
  }

}
