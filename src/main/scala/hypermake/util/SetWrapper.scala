package hypermake.util

import scala.collection._


trait SetWrapper[K] extends Set[K] {
  def underlying: Set[K]

  def diff(that: Set[K]) = underlying diff that

  def contains(elem: K) = underlying contains elem
  def iterator = underlying.iterator
}
