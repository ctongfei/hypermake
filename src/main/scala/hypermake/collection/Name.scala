package hypermake.collection

import cats._

/**
 * Represents a variable that serves as an axis to a parameterized value or task.
 */
case class Name(name: String) {
  override def toString = name

  override def hashCode() = name.hashCode
}

object Name {

  implicit object Ordering extends Ordering[Name] {
    def compare(x: Name, y: Name) = x.name compare y.name
  }

  implicit object Order extends Order[Name] {
    def compare(x: Name, y: Name) = x.name compare y.name
  }

}
