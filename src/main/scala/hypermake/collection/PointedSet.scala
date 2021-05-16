package hypermake.collection

import hypermake.exception.AxesDefaultKeyMismatchException

import scala.collection._

/**
 * A set with a default element.
 */
trait PointedSet[A] extends Set[A] {

  /** The default element in this set. */
  def default: A

  def underlying: Set[A]

  def contains(elem: A) = underlying contains elem
  def diff(that: Set[A]) = underlying diff that
  def iterator = underlying.iterator

  def intersect(that: PointedSet[A]): PointedSet[A] = {
    if (this.default != that.default)
      throw AxesDefaultKeyMismatchException(None, this.default.toString, that.default.toString)
    PointedSet(this.underlying intersect that.underlying, this.default)
  }

  def subsetOf(that: PointedSet[A]) = {
    if (this.default != that.default)
      throw AxesDefaultKeyMismatchException(None, this.default.toString, that.default.toString)
    this.underlying subsetOf that.underlying
  }

}

object PointedSet {

  def apply[A](underlyingSet: Set[A], defaultElem: A): PointedSet[A] = {
    assert(underlyingSet contains defaultElem)
    new PointedSet[A] {
      def default = defaultElem
      def underlying = underlyingSet
    }
  }

}
