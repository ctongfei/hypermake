package hypermake.semantics

import hypermake.core._
import hypermake.exception.ParametersUnboundException

import scala.collection._

case class Decorator(name: Name, params: Set[Name], inputScriptParam: Name, outputScript: Script) {

  def reify(args: Map[Name, Value]): (Name, Script) = {
    val reified = partiallyReify(args)
    if (reified.params.isEmpty)
      (inputScriptParam, reified.outputScript)
    else throw ParametersUnboundException(reified.params, name)
  }

  def partiallyReify(args: Map[Name, Value]) = {
    val unboundParams = params diff args.keySet
    Decorator(name, unboundParams, inputScriptParam, outputScript.withNewArgs(args))
  }

}

object Decorator {

}