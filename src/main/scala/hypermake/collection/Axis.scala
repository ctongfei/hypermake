package hypermake.collection

import cats._

/** Represents a variable that serves as an axis to a parameterized value or task.
  */
case class Axis(name: String) {
  override def toString = name

  override def hashCode() = name.hashCode
}

object Axis {

  implicit object Ordering extends Ordering[Axis] {
    def compare(x: Axis, y: Axis) = x.name compare y.name
  }

  implicit object Order extends Order[Axis] {
    def compare(x: Axis, y: Axis) = x.name compare y.name
  }

}
