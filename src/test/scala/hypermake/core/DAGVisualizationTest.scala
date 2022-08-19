package hypermake.core

import hypermake.collection.Graph

object DAGVisualizationTest extends App {

  val g = Graph[Int]()
  g.addNode(1)
  g.addNode(2)
  g.addNode(3)
  g.addNode(4)
  g.addNode(5)
  g.addArc(1, 2)
  g.addArc(1, 3)
  g.addArc(1, 4)
  g.addArc(2, 3)
  g.addArc(2, 5)

  val indents = g.computeIndentation
  println(indents)

}
