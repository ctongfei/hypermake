package hypermake.collection

import scala.collection._
import hypermake.exception._
import hypermake.util._


class Graph[A](
                val adjMap: mutable.HashMap[A, mutable.HashSet[A]],
                val revAdjMap: mutable.HashMap[A, mutable.HashSet[A]]
              ) {

  def nodes: Set[A] = adjMap.keySet
  def numNodes = nodes.size

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

  def topologicalSort: Iterable[A] = new Iterable[A] {
    def iterator: Iterator[A] = new Iterator[A] {
      private[this] val inDegrees = mutable.HashMap.from(nodes.makeMap(incomingNodes(_).size))
      private[this] val zeroInDegrees = mutable.Queue.from(inDegrees.view.filter(_._2 == 0).keys)
      private[this] var n = 0
      def hasNext = n < nodes.size
      def next() = {
        if (zeroInDegrees.isEmpty) throw CyclicWorkflowException()
        val i = zeroInDegrees.dequeue()
        for (j <- outgoingNodes(i)) {
          inDegrees(j) -= 1
          if (inDegrees(j) == 0)
            zeroInDegrees.enqueue(j)
        }
        n += 1
        i
      }
    }
  }
//
//   def toStringIfAcyclic: String = {
//     val inDegrees = mutable.HashMap.from(nodes.makeMap(incomingNodes(_).size))
//     val zeroInDegrees = mutable.HashSet.from(inDegrees.view.filter(_._2 == 0).keys)
//     val indents = mutable.HashMap.from(nodes.makeMap(_ => -1))
//     val buffer = mutable.ArrayBuffer[A]()
//
//     }
//
//     val lines = Array.fill(numNodes)(new mutable.StringBuilder())
//     for ((v, i) <- sorted.zipWithIndex) {
//       lines(i).append(" " * indents(v))
//       lines(i).append("• ")
//       lines(i).append(v.toString)
//     }
//     lines.map(_.toString()).mkString("\n")
//   }

}

object Graph {

  def apply[A]() = new Graph[A](
    mutable.HashMap[A, mutable.HashSet[A]](),
    mutable.HashMap[A, mutable.HashSet[A]]()
  )

  /**
   * Performs a traversal to resolve all dependent tasks of the given targets.
   * @param sources A collection of target tasks
   * @return The task dependency DAG
   */
  def explore[A](sources: Iterable[A], next: A => Iterable[A]): Graph[A] = {
    val g = Graph[A]()
    val s = mutable.HashSet[A]()
    val q = mutable.Queue.from(sources)

    while (q.nonEmpty) {
      val c = q.dequeue()
      if (!s.contains(c)) {
        g.addNode(c)
        s.add(c)
        for (d <- next(c)) {
          g.addNode(d)
          g.addArc(d, c)
          q.enqueue(d)
        }
      }
    }
    g
  }

}
