package hypermake.core

import scala.collection._

import hypermake.collection._
import hypermake.exception._
import hypermake.util._

/**
 * Encapsulates a set of parameters, with potential default values.
 * @param params A map from parameter names to their default values.
 *               If a parameter is not present in the map, it is considered unbound.
 */
case class Params[+V <: Value](params: Map[String, Option[PointedTensor[V]]]) {

  def hasUnboundVars = params.values.exists(_.isEmpty)

  def boundVars = params.collect { case (k, Some(v)) => k -> v }.toMap

  def unboundVars = params.collect { case (k, None) => k }.toSet

  def ++[W >: V <: Value](that: Params[W]) = Params(this.params ++ that.params)

  def bind(args: PointedArgsTensor[Value]): Params[Value] =
    Params(params.map { case (k, v) => k -> args.args.get(k).orElse(v) })

  def bind(args: Args[Value]): Params[Value] =
    Params(params.map { case (k, v) => k -> args.get(k).map(v => PointedTensor.Singleton(v)).orElse(v) })

  def bind(args: (String, String)*): Params[Value] = bind(Args(args.toMap.mapValuesE(Value.Pure)))

  def toArgsIfBound(funcName: Option[String] = None) =
    PointedArgsTensor(params.mapValuesE(_.getOrElse(throw ParametersUnboundException(unboundVars, funcName))))

}

object Params {

  def fromArgs[V <: Value](args: PointedArgsTensor[V]): Params[V] = Params(args.args.mapValuesE(Some(_)))

}
