package hypermake.core

import scala.collection._
import hypermake.collection._
import hypermake.syntax.Def

case class Module(
    name: Name,
    params: Map[Name, PointedCube[Value]],
    defs: Seq[Def],
    inheritedArgs: Map[Name, PointedCube[Value]] = Map()
) {
  def withNewArgs(args: Map[Name, PointedCube[Value]]) = {
    Module(name, params, defs, inheritedArgs ++ args)
  }
}
