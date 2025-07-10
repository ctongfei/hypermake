package hypermake.core

import scala.collection._

import hypermake.collection._
import hypermake.exception.ParametersUnboundException
import hypermake.semantics.Definition
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
      (obj.defs.map {
        case Definition(name, p: Partial[_]) => name -> Definition(name, p.partial(args))
        case d: Definition[_]                => d.name -> d // values and objects do not take arguments
      }.toMap ++ newArgs.args.map { case (k, v) =>
        val d = Definition(k, v)
        d.name -> d
      }.toMap).values
    )
    Cls(name, params, newObj, newArgs)
  }

  /**
   * Instantiate this class with the given arguments and returns an object.
   * @param args Arguments passed to the constructor of this class
   * @return The instantiated object
   */
  def instantiate(args: PointedArgsTensor[Value], name: String): Obj = {
    val newCls = partial(args).copy(name = name)
    if (newCls.params.hasUnboundVars)
      throw new ParametersUnboundException(newCls.params.unboundVars, Some(name))
    newCls.obj
  }

}
