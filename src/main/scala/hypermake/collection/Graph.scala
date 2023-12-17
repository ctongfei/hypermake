package hypermake.collection

import hypermake.exception._
import hypermake.util._

import scala.collection._

trait Graph[A] {
  self =>

  def adjMap: Map[A, Set[A]]

  def revAdjMap: Map[A, Set[A]]

  def nodes: Set[A]

  def numNodes = nodes.size

  def containsNode(a: A) = adjMap.contains(a)

  def containsArc(a: A, b: A): Boolean = adjMap.contains(a) && adjMap(a).contains(b)

  def outgoingNodes(a: A): Set[A] = adjMap(a)

  def incomingNodes(a: A): Set[A] = revAdjMap(a)

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

  def traverseBothDirections(start: A): Iterable[A] = new Iterable[A] {
    def iterator: Iterator[A] = new Iterator[A] {
      private[this] val visited = mutable.HashSet.empty[A]
      private[this] val queue = mutable.Queue[A](start)

      def hasNext = queue.nonEmpty

      def next() = {
        val c = queue.dequeue()
        visited += c
        (outgoingNodes(c) | incomingNodes(c)).filterNot(visited).foreach(queue.enqueue)
        c
      }
    }
  }

  def subgraph(subgraphNodes: Set[A]): Graph[A] = new Graph[A] {
    def adjMap = self.adjMap.filterKeysL(subgraphNodes).mapValuesL(_ & subgraphNodes)

    def revAdjMap = self.revAdjMap.filterKeysL(subgraphNodes).mapValuesL(_ & subgraphNodes)

    def nodes = self.nodes & subgraphNodes
  }

  def weaklyConnectedComponents: Iterable[Graph[A]] = new Iterable[Graph[A]] {
    def iterator: Iterator[Graph[A]] = new Iterator[Graph[A]] {
      private[this] val visited = mutable.HashSet[A]()
      private[this] val nodeIter = self.nodes.iterator

      def hasNext = visited.size < nodes.size

      def next: Graph[A] = {
        while (nodeIter.hasNext) {
          val a = nodeIter.next
          if (!visited.contains(a)) {
            val subgraphNodes = traverseBothDirections(a).toSet
            visited ++= subgraphNodes
            return subgraph(subgraphNodes)
          }
        }
        throw new IllegalStateException() // should never happen
      }
    }
  }

  def computeIndentation = {
    val rows = mutable.ArrayBuffer[A]()
    val nodeToRow = mutable.HashMap[A, Int]()
    val indents = mutable.ArrayBuffer[Int]()

    def processDag(g: Graph[A], indent: Int): Unit = {
      for (sg <- g.weaklyConnectedComponents)
        processComponent(sg, indent)
    }

    def processComponent(g: Graph[A], indent: Int): Unit = {
      val sources = g.nodes.filter(g.incomingNodes(_).isEmpty)
      for ((s, i) <- sources.toSeq.zipWithIndex) {
        nodeToRow(s) = rows.size
        rows += s
        indents += (indent + i)
      }
      processDag(g.subgraph(g.nodes &~ sources), indent + sources.size)
    }

    processDag(this, 0)
    (rows, nodeToRow, indents)
  }

  def toStringIfAcyclic(display: A => HIO[String], indent: Int = 0): HIO[String] = {
    import hypermake.util.printing.BoxDrawing._
    import zio._
    val prefixSpaces = " " * indent
    topologicalSort.toSeq.foreach(_ => {}) // throw error if not acyclic
    val (rows, nodeToRow, indents) = computeIndentation
    val a = Array.tabulate(rows.size)(i => Array.fill(indents(i) * 2)(' '))
    for ((u, i) <- rows.zipWithIndex) {
      val succ = outgoingNodes(u).toSeq.sortBy(nodeToRow)
      if (succ.nonEmpty) {
        val l = nodeToRow(succ.last) // row of last successor
        for (j <- (i + 1) until l)
          a(j)(indents(i) * 2) = V
        for (v <- succ.init) {
          val j = nodeToRow(v)
          a(j)(indents(i) * 2) = if (indents(i) > 0 && a(j)(indents(i) * 2 - 1) == H) VH else VR
          for (k <- (indents(i) * 2 + 1) until indents(j) * 2)
            a(j)(k) = H
        }
        a(l)(indents(i) * 2) = if (indents(i) > 0 && a(l)(indents(i) * 2 - 1) == H) HT else TR
        for (k <- (indents(i) * 2 + 1) until indents(l) * 2)
          a(l)(k) = H
      }
    }
    ZIO
      .foreach(a zip rows)({ case (row, x) => display(x).map(s => prefixSpaces + s"${String.valueOf(row)}$s") })
      .map(_.mkString("\n"))
  }
}

class MutableGraph[A](
    val adjMap: mutable.HashMap[A, mutable.HashSet[A]],
    val revAdjMap: mutable.HashMap[A, mutable.HashSet[A]]
) extends Graph[A] {

  def nodes: Set[A] = adjMap.keySet

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

}

object Graph {

  /** Performs a traversal to resolve all dependent tasks of the given targets.
    *
    * @param sources
    *   A collection of target tasks
    * @return
    *   The task dependency DAG
    */
  def explore[A](sources: Iterable[A], prev: A => Iterable[A]): Graph[A] = {
    val g = Graph[A]()
    val s = mutable.HashSet[A]()
    val q = mutable.Queue.from(sources)

    while (q.nonEmpty) {
      val c = q.dequeue()
      if (!s.contains(c)) {
        g.addNode(c)
        s.add(c)
        for (d <- prev(c)) {
          g.addNode(d)
          g.addArc(d, c)
          q.enqueue(d)
        }
      }
    }
    g
  }

  def apply[A]() = new MutableGraph[A](
    mutable.HashMap[A, mutable.HashSet[A]](),
    mutable.HashMap[A, mutable.HashSet[A]]()
  )

}
