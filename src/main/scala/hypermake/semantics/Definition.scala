package hypermake.semantics

import hypermake.collection.Path

case class Definition[+T](name: Path, value: T)

object Definition {

  def apply[T](name: String, value: T): Definition[T] =
    new Definition(Path(name.split('.').toList), value)

}
