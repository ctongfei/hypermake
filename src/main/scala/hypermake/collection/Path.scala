package hypermake.collection

import scala.collection._
import hypermake.util.DefaultMapBase

/** An identifier path in the format of "a.b.c".
  *
  * @param components
  */
case class Path(val components: List[String]) {
  override def toString = components.mkString(".")

  def head = components.head
  def tail = Path(components.tail)
  def init = Path(components.init)
  def last = components.last

}

class PathMap[R, +A](root: R, children: R => Map[String, R], base: R => Map[String, A])
    extends DefaultMapBase[Path, A] {

  def childMap(key: String): PathMap[R, A] = children(root)
    .get(key)
    .map(child => new PathMap(child, children, base))
    .getOrElse(PathMap.empty)

  def get(key: String): Option[A] = get(Path(key.split('.').toList))

  def get(key: Path): Option[A] = {
    val head :: tail = key.components
    if (tail.isEmpty) base(root).get(head)
    else childMap(head).get(Path(tail))
  }

  def apply(key: String): A = apply(Path(key.split('.').toList))

  def childrenIterator: Iterator[(Path, R)] = new Iterator[(Path, R)] {
    private[this] val stack = mutable.Stack[(List[String], R)](Nil -> root)

    def hasNext = stack.nonEmpty

    def next() = {
      val (path, obj) = stack.pop()
      val descendents = children(obj)
      stack.pushAll(descendents.map { case (k, v) => (k :: path) -> v })
      Path(path.reverse) -> obj
    }
  }

  def iterator: Iterator[(Path, A)] = childrenIterator.flatMap { case (path, obj) =>
    base(obj).map { case (k, v) => Path(path.components :+ k) -> v }
  }

}

object PathMap {
  def empty[R, A] = new PathMap[R, A](null.asInstanceOf[R], _ => Map(), _ => Map())
}
