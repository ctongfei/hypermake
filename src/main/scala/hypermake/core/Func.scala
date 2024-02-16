package hypermake.core

import scala.collection._

import cats.instances.map._
import cats.syntax.unorderedTraverse._
import hypermake.collection._
import hypermake.exception.ParametersUnboundException

case class Func(
    name: String,
    params: Set[String],
    impl: Script
) {

  /** Constructs the complete script with the unbound variables assigned.
    */
  def reify(args: Map[String, Value]): Script = {
    val reified = withNewArgs(args)
    if (reified.params.nonEmpty)
      throw new ParametersUnboundException(reified.params, reified.name)
    reified.impl
  }

  /** Fills in some of the parameters of this function (partial application).
    */
  def withNewArgs(args: Map[String, Value]): Func = {
    val unboundParams = params.filter(a => !args.contains(a))
    Func(name, unboundParams, impl.withNewArgs(args))
  }

}

class PointedFuncTensor(
    val name: String,
    val cases: PointedCaseTensor,
    val params: Set[String],
    val impl: PointedTensor[Script]
) extends PointedTensor[Func] { self =>

  def get(c: Case): Option[Func] = {
    if (cases containsCase c) {
      val scr = impl.select(c).default
      Some(Func(name, params, scr))
    } else None
  }

  def withNewArgs(args: Map[String, PointedTensor[Value]]): PointedFuncTensor = {
    val unboundParams = params.filter(a => !args.contains(a))
    val outScript = impl.productWith(args.toMap.unorderedSequence)(_ withNewArgs _)
    new PointedFuncTensor(name, cases, unboundParams, outScript)
  }

}
