package hypermake.collection

import scala.collection._

import hypermake.util.DefaultMapBase

/** A prefix trie-based map whose keys are paths. */
trait PathMap[+A] extends DefaultMapBase[Path, A] { self =>

  /** The existential type of namespaces in the hierarchy of namespaces. */
  type Namespace
  def root: Namespace
  def children(obj: Namespace): Map[String, Namespace]
  def leaves(obj: Namespace): Map[String, A]

  def childMap(key: String): PathMap[A] = children(root)
    .get(key)
    .map(child => PathMap(child, children, leaves))
    .getOrElse(PathMap.empty)

  def get(key: String): Option[A] = get(Path(key.split('.').toList))

  def get(key: Path): Option[A] = {
    if (key.components.isEmpty) None
    else {
      val head :: tail = key.components
      if (tail.isEmpty) leaves(root).get(head)
      else childMap(head).get(Path(tail))
    }
  }

  def apply(key: String): A = apply(Path(key.split('.').toList))

  def childrenIterator: Iterator[(Path, Namespace)] = new Iterator[(Path, Namespace)] {
    private[this] val stack = mutable.Stack[(List[String], Namespace)](Nil -> root)

    def hasNext: Boolean = stack.nonEmpty

    def next(): (Path, Namespace) = {
      val (path, obj) = stack.pop()
      val descendents = children(obj)
      stack.pushAll(descendents.map { case (k, v) => (k :: path) -> v })
      Path(path.reverse) -> obj
    }
  }

  def iterator: Iterator[(Path, A)] = childrenIterator.flatMap { case (path, obj) =>
    leaves(obj).map { case (k, v) => Path(path.components :+ k) -> v }
  }

  def ++[B >: A](that: PathMap[B]): DefaultMapBase[Path, B] = new DefaultMapBase[Path, B] {
    def get(key: Path) = self.get(key).orElse(that.get(key))
    def iterator = ???
  }
}

object PathMap {
  def empty[N, A] = apply[N, A](null.asInstanceOf[N], _ => Map(), _ => Map())

  def apply[N, A](
      fRoot: N,
      fChildren: N => Map[String, N],
      fLeaves: N => Map[String, A]
  ): PathMap[A] = new PathMap[A] {
    type Namespace = N
    def root = fRoot
    def children(obj: N) = fChildren(obj)
    def leaves(obj: N) = fLeaves(obj)
  }
}
