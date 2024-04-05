package hypermake.core

import scala.collection._

import cats.syntax.unorderedTraverse._

import hypermake.collection.{Case, PointedShape, PointedTensor}
import hypermake.exception._
import hypermake.util._

/**
 * Encapsulates a set of parameters, with potential default values.
 * @param params A map from parameter names to their default values.
 *               If a parameter is not present in the map, it is considered unbound.
 */
case class Params[+V <: Value](params: Map[String, Option[PointedTensor[V]]]) {

  def hasUnboundVars = params.values.exists(_.isEmpty)

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

case class Args[+V <: Value](args: Map[String, V]) extends DefaultMapBase[String, V] {

  def get(key: String): Option[V] = args.get(key)

  def iterator: Iterator[(String, V)] = args.iterator

  def ++[W >: V <: Value](that: Args[W]) = Args(this.args ++ that.args)

  def withArgs(newArgs: (String, String)*) =
    Args(newArgs.toMap.mapValuesE(Value.Pure))

  def toStrMap = args.map { case (k, v) => k -> v.value }

  def toShell = args.map { case (k, v) => s"$k=${Escaper.Shell.escape(v.value)}" }.mkString("\n")

}

case class PointedArgsTensor[+V <: Value](args: Map[String, PointedTensor[V]]) extends PointedTensor[Args[V]] { self =>

  def shape = args.values.map(_.shape).fold(PointedShape.singleton)(_ outerJoin _)

  def ++[W >: V <: Value](that: PointedArgsTensor[W]) = PointedArgsTensor(this.args ++ that.args)

  def get(c: Case): Option[Args[V]] = {
    if (shape containsCase c) {
      val values = args.map { case (k, v) => k -> v.select(c).default }
      Some(Args(values))
    } else None
  }

}
