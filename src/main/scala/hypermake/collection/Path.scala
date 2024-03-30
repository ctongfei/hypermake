package hypermake.collection

import scala.collection._

import hypermake.util.DefaultMapBase

/** An identifier path in the format of "a.b.c". */
case class Path(components: List[String]) {
  override def toString = components.mkString(".")

  def head = components.head
  def tail = Path(components.tail)
  def init = Path(components.init)
  def last = components.last

  def /(c: String) = Path(components :+ c)

}

object Path {
  val root = Path()
  def apply(components: String*): Path = Path(components.toList)
}
