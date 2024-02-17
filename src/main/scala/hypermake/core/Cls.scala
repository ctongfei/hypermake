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
                params: Map[String, PointedTensor[Value]],
                obj: Obj,
                inheritedArgs: Map[String, PointedTensor[Value]] = Map()
) {
  def withNewArgs(args: Map[String, PointedTensor[Value]]): Cls = {
    val newObj = Obj.fromDefs(
      obj.defs.map {
        case Definition(name, pct: PointedTaskTensor) =>
          Definition(name, pct.withNewArgs(args))
        case Definition(name, pcp: PointedPackageTensor) =>
          Definition(name, pcp.withNewArgs(args))
        case Definition(name, pft: PointedFuncTensor) =>
          Definition(name, pft.withNewArgs(args))
        case Definition(name, value: Cls) =>
          Definition(name, value.withNewArgs(args))
        case d: Definition[_] => d  // values and objects
      }
    )
    Cls(name, params, newObj, inheritedArgs ++ args)
  }

  def instantiate(args: Map[String, PointedTensor[Value]]): Obj = {
    val unboundParams = params.filterKeysE(a => !args.contains(a)).keySet
    if (unboundParams.nonEmpty)
      throw new ParametersUnboundException(unboundParams, name)
    withNewArgs(args).obj
  }

}
