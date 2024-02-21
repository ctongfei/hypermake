package hypermake.semantics

import hypermake.collection.Path


case class Definition[+T](name: Path, value: T)

object Definition {

  def apply[T](name: String, value: T): Definition[T] = Definition(Path(List(name)), value)

}
