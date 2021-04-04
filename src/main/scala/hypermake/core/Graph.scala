package hypermake.core

import hypermake.exception.CyclicWorkflowException

import scala.collection._

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
    val inDegrees = mutable.HashMap.from(nodes.map(a => (a, incomingNodes(a).size)))
    val zeroInDegrees = mutable.HashSet.from(inDegrees.filter(_._2 == 0).keys)
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

  def apply[A]() = new Graph[A](mutable.HashMap[A, mutable.HashSet[A]](), mutable.HashMap[A, mutable.HashSet[A]]())

}
