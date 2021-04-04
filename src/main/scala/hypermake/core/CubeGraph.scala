package hypermake.core

import hypermake.util._

import scala.collection._

class CubeGraph[V, E](
                      axes: CaseCube,
                      nodes: Map[Name, Cube[V]],
                      edges: Map[Name, Cube[E]]
) { self =>

  def restrict(restrictions: CaseCube): CubeGraph[V, E] = {
    val newAxes = axes outerJoin restrictions
    val newNodes = nodes.mapValuesE(_.selectMany(newAxes))
    val newEdges = edges.mapValuesE(_.selectMany(newAxes))
    new CubeGraph[V, E](newAxes, newNodes, newEdges)
  }
  

}

//
//class PointedHyperDAG[V, E](
//                             axes: Map[Var, PointedSet[String]],
//                             nodes: Map[Var, PointedCube[V]],
//                             edges: Map[Var, PointedCube[E]]
//                           ) extends HyperDAG[V, E](axes, nodes, edges)
//{ self =>
//
//  def restrictUnderPlan(cases: Map[Var, Set[String]]): HyperDAG[V, E] = {
//    val newAxes = Util.joinCases(self.axes, cases)
//    val newNodes = nodes.mapValuesE(_.curry(newAxes.keySet).default.selectMany(cases))
//    val newEdges = edges.mapValuesE(_.curry(newAxes.keySet).default.selectMany(cases))
//    new HyperDAG[V, E](newAxes, newNodes, newEdges)
//  }
//
//}
