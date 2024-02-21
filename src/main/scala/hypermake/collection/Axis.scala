package hypermake.collection

import cats._

/** Represents a variable that serves as an axis to a parameterized value or task.
  */
case class Axis(name: String) {
  override def toString = name

  override def hashCode() = name.hashCode
}

object Axis {

  implicit object Eq extends Order[Axis] with Hash[Axis] {

    override def hash(x: Axis): Int = x.name.hashCode

    def compare(x: Axis, y: Axis) = x.name compare y.name
  }

  implicit val ordering: Ordering[Axis] = Eq.toOrdering

}
