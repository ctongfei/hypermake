package hypermake.core

import scala.collection._

import hypermake.collection._
import hypermake.exception.ParametersUnboundException
import hypermake.semantics.{Context, Definition}
import hypermake.syntax.ast._
import hypermake.util._

/**
 * Represents a Hypermake class.
 * @param name Name of this class, from root
 * @param params Constructor parameters, with their default values
 * @param obj The object definition of this class
 * @param inheritedArgs Arguments inherited from a partial instantiation
 */
case class Cls(
    name: String,
    params: Params[Value],
    obj: Obj,
    inheritedArgs: PointedArgsTensor[Value] = PointedArgsTensor(Map())
) extends Partial[Cls] {
  def partial(args: PointedArgsTensor[Value]): Cls = {
    val newArgs = PointedArgsTensor(inheritedArgs.args ++ args.args)
    val newObj = Obj.fromDefs(
      obj.prefix,
      obj.defs.map {
        case Definition(name, pct: PointedTaskTensor) =>
          Definition(name, pct.partial(args))
        case Definition(name, pcp: PointedPackageTensor) =>
          Definition(name, pcp.partial(args))
        case Definition(name, pft: Func) =>
          Definition(name, pft.partial(args))
        case Definition(name, value: Cls) =>
          Definition(name, value.partial(args))
        case d: Definition[_] => d // values and objects
      } ++ newArgs.args.map { case (k, v) => Definition(k, v) }
    )
    Cls(name, params, newObj, newArgs)
  }

  /**
   * Instantiate this class with the given arguments and returns an object.
   * @param args Arguments passed to the constructor of this class
   * @return The instantiated object
   */
  // TODO: name of the object
  def instantiate(args: PointedArgsTensor[Value]): Obj = {
    val newCls = partial(args)
    if (newCls.params.hasUnboundVars)
      throw new ParametersUnboundException(newCls.params.unboundVars, Some(name))
    newCls.obj
  }

}
