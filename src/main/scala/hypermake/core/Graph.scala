package hypermake.core

import scala.collection._
import hypermake.exception._
import hypermake.util._


class Graph[A](
                val adjMap: mutable.HashMap[A, mutable.HashSet[A]],
                val revAdjMap: mutable.HashMap[A, mutable.HashSet[A]]
              ) {

  def nodes: Set[A] = adjMap.keySet
  def containsNode(a: A) = adjMap.contains(a)
  def containsArc(a: A, b: A): Boolean = adjMap.contains(a) && adjMap(a).contains(b)

  def outgoingNodes(a: A): Set[A] = adjMap(a)
  def incomingNodes(a: A): Set[A] = revAdjMap(a)

  def addNode(a: A): Unit = {
    if (!adjMap.contains(a)) {
      adjMap += a -> mutable.HashSet()
      revAdjMap += a -> mutable.HashSet()
    }
  }

  def addArc(a: A, b: A): Unit = {
    adjMap(a).add(b)
    revAdjMap(b).add(a)
  }

  def topologicalSort: Seq[A] = {
    val inDegrees = mutable.HashMap.from(nodes.makeMap(incomingNodes(_).size))
    val zeroInDegrees = mutable.HashSet.from(inDegrees.view.filter(_._2 == 0).keys)
    val buffer = mutable.ArrayBuffer[A]()
    var n = 0
    while (n < nodes.size) {
      val i = zeroInDegrees.headOption.getOrElse(throw CyclicWorkflowException())
      zeroInDegrees -= i
      buffer += i
      for (j <- outgoingNodes(i)) {
        inDegrees(j) -= 1
        if (inDegrees(j) == 0)
          zeroInDegrees += j
      }
      n += 1
    }
    buffer
  }

}

object Graph {

  def apply[A]() = new Graph[A](
    mutable.HashMap[A, mutable.HashSet[A]](),
    mutable.HashMap[A, mutable.HashSet[A]]()
  )

  /**
   * Performs a traversal to resolve all dependent tasks of the given targets.
   * @param targets A collection of target tasks
   * @return The task dependency DAG
   */
  def resolveFromTargets[A](targets: Iterable[A], predecessors: A => Iterable[A]): Graph[A] = {
    val g = Graph[A]()
    val s = mutable.HashSet[A]()
    val q = mutable.Queue.from(targets)

    while (q.nonEmpty) {
      val c = q.dequeue()
      if (!s.contains(c)) {
        g.addNode(c)
        s.add(c)
        for (d <- predecessors(c)) {
          g.addNode(d)
          g.addArc(d, c)
          q.enqueue(d)
        }
      }
    }
    g
  }

}
