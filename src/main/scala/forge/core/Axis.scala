package forge.core


class Axis(val name: String, val keys: Seq[String]) {

  override def toString = s"{$name: ${keys.mkString(" ")}}"

}

object Axis {

  def apply(name: String, keys: Seq[String]): Axis =
    new Axis(name, keys)

}
