package hypermake.collection

import scala.collection._

import hypermake.syntax.ast.Identifier
import hypermake.util.MapWrapper

/** Encapsulates the parameters of a specific case of a variable or task. */
class Case(val underlying: Map[Axis, String]) {

  def vars = underlying.keySet

  def apply(a: Axis) = underlying(a)

  def get(a: Axis) = underlying get a

  def contains(a: Axis) = underlying contains a

  def assignments: Iterable[(Axis, String)] = underlying

  def ++(that: Case) = Case(this.underlying ++ that.underlying)

  override def toString = underlying.map { case (a, k) => s"$a: $k" }.mkString("[", ", ", "]")

}

object Case {
  def apply(underlying: Map[Axis, String]) = new Case(underlying)

  def from(indices: (Axis, String)*) = new Case(indices.toMap)
}
