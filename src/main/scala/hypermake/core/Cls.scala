package hypermake.core

import scala.collection._
import hypermake.collection._
import hypermake.exception.ParametersUnboundException
import hypermake.semantics.{Context, Definition}
import hypermake.util._
import hypermake.syntax.ast._

/** Represents a Hypermake class.
  * @param name
  * @param params
  * @param defs
  * @param inheritedArgs
  */
case class Cls(
    name: String,
    params: Map[String, PointedCube[Value]],
    obj: Obj,
    inheritedArgs: Map[String, PointedCube[Value]] = Map()
) {
  def withNewArgs(args: Map[String, PointedCube[Value]]): Cls = {
    val newObj = Obj.fromDefs(obj.defs.map {
      case Definition(name, pct: PointedCubeTask) =>
        Definition(name, pct.withNewArgs(args))
      case Definition(name, pcp: PointedCubePackage) =>
        Definition(name, pcp.withNewArgs(args))
      case Definition(name, f: Func) =>
        Definition(name, f.withNewArgs(args))
      case Definition(name, value: Cls) =>
        Definition(name, value.withNewArgs(args))
      case d: Definition[_] => d
    })
    Cls(name, params, newObj, inheritedArgs ++ args)
  }

  def instantiate(args: Map[String, PointedCube[Value]]): Obj = {
    val unboundParams = params.filterKeysE(a => !args.contains(a)).keySet
    if (unboundParams.nonEmpty)
      throw new ParametersUnboundException(unboundParams, name)
    withNewArgs(args).obj
  }

}
