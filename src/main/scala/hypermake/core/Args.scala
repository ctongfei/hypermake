package hypermake.core

import scala.collection._

import hypermake.collection._
import hypermake.util._

/** Represents a set of arguments that can be passed to anything that takes [[Params]]. */
case class Args[+V <: Value](args: Map[String, V]) extends DefaultMapBase[String, V] {

  def get(key: String): Option[V] = args.get(key)

  def iterator: Iterator[(String, V)] = args.iterator

  def ++[W >: V <: Value](that: Args[W]) = Args(this.args ++ that.args)

  def withArgs(newArgs: (String, String)*) =
    Args(newArgs.toMap.mapValuesE(Value.Pure))

  def toStrMap = args.map { case (k, v) => k -> v.value }

  /** Converts this to a shell string that can be prepended to a Shell command. */
  def toShell = args.map { case (k, v) => s"$k=${Escaper.Shell.escape(v.value)}" }.mkString("\n")

}

object Args {

  def from(args: (String, String)*): Args[Value] = Args(args.toMap.mapValuesE(Value.Pure))

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
