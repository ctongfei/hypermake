package hypermake.core

import scala.collection._

import hypermake.collection._

/** Represents a collection of slices of tasks whose execution can be invoked from the command line. */
case class Plan(val targets: Seq[Tensor[Job]]) {

  /**
   * Performs a traversal to resolve all dependent tasks of a given plan.
   * @return The task dependency DAG
   */
  def dependencyGraph: Graph[Job] = {
    val targetJobs = targets.flatMap(_.allElements)
    Graph.explore(targetJobs, _.dependentJobs)
  }

}
