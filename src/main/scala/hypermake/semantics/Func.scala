package hypermake.semantics

import hypermake.core.Name

import scala.collection._
import hypermake.exception._
import hypermake.semantics.Escaper._
import hypermake.syntax.Identifier

/**
 * Defines a Forge function.
 * @param name
 * @param params
 * @param script
 */
case class Func(name: Name, params: Set[Name], script: Script) {

  /**
   * Constructs the complete script with the unbound variables assigned.
   */
  def reify(args: Map[Name, Value]) = {
    val reified = partiallyReify(args)
    if (reified.params.isEmpty)
      reified.script
    else throw ParametersUnboundException(reified.params, name)
  }

  /**
   * Fills in some of the parameters of this function (currying).
   */
  def partiallyReify(args: Map[Name, Value]): Func = {
    val unboundParams = params diff args.keySet
    Func(name, unboundParams, script.withNewArgs(args))
  }

  override def toString = f"def $name(${params.mkString(", ")}):\n$script"

}
