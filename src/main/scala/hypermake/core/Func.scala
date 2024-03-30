package hypermake.core

import scala.collection._

import cats.implicits.catsStdInstancesForMap
import cats.syntax.unorderedTraverse._

import hypermake.collection._
import hypermake.exception.ParametersUnboundException

// TODO: enable default parameters
/** Encapsulates a reusable snippet of code across tasks. */
case class Func(
    name: String,
    params: Set[String],
    outputs: Set[String],
    impl: Script
) {

  /** Constructs the complete script with the unbound variables assigned. */
  def reify(args: Map[String, Value]): Script = {
    val reified = withNewArgs(args)
    if (reified.params.nonEmpty)
      throw ParametersUnboundException(reified.params, reified.name)
    reified.impl
  }

  /** Fills in some of the parameters of this function (partial application). */
  def withNewArgs(args: Map[String, Value]): Func = {
    val unboundParams = params.filter(a => !args.contains(a))
    val unboundOutputs = outputs.filter(a => !args.contains(a))
    Func(name, unboundParams, unboundOutputs, impl.withNewArgs(args))
  }

}

case class PointedFuncTensor(
    name: String,
    params: Set[String],
    outputs: Set[String],
    impl: PointedTensor[Script]
) extends PointedTensor[Func] { self =>

  def shape = impl.shape

  def get(c: Case): Option[Func] = {
    if (impl.shape containsCase c) {
      val scr = impl.select(c).default
      Some(Func(name, params, outputs, scr))
    } else None
  }

  def withNewArgs(args: Map[String, PointedTensor[Value]]): PointedFuncTensor = {
    val unboundInputParams = params.filter(a => !args.contains(a))
    val unboundOutputParams = outputs.filter(a => !args.contains(a))
    val outScript = impl.productWith(args.toMap.unorderedSequence)(_ withNewArgs _)
    new PointedFuncTensor(name, unboundInputParams, unboundOutputParams, outScript)
  }

}
