package hypermake.core

import hypermake.syntax.Identifier
import hypermake.util.MapWrapper

import scala.collection._

class Case(val underlying: Map[Name, String]) {

  def vars = underlying.keySet

  def apply(a: Name) = underlying(a)
  def get(a: Name) = underlying get a

  def contains(a: Name) = underlying contains a

  def assignments: Iterable[(Name, String)] = underlying

  def ++(that: Case) = Case(this.underlying ++ that.underlying)

}

object Case {
  def apply(underlying: Map[Name, String]) = new Case(underlying)
  def from(indices: (Name, String)*) = new Case(indices.toMap)
}
