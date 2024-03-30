package hypermake.collection

import scala.collection._

import cats.Hash
import cats.implicits._
import cats.syntax.hash._

import hypermake.exception.AxesDefaultKeyMismatchException

/** A set with a default element. */
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

  def equals(that: PointedSet[A]) = {
    if (this.default != that.default)
      throw AxesDefaultKeyMismatchException(None, this.default.toString, that.default.toString)
    this.underlying equals that.underlying
  }

  override def hashCode() = (default, underlying).hashCode()

}

object PointedSet {

  def apply[A](underlyingSet: Set[A], defaultElem: A): PointedSet[A] = {
    assert(underlyingSet contains defaultElem)
    new PointedSet[A] {
      def default = defaultElem

      def underlying = underlyingSet
    }
  }

  // does not utilize `cats.Hash`: just uses Scala set with default equality and hash
  implicit def Hash[A]: Hash[PointedSet[A]] = new Hash[PointedSet[A]] {
    def hash(x: PointedSet[A]) = x.hashCode()
    def eqv(x: PointedSet[A], y: PointedSet[A]): Boolean = x equals y
  }

}
