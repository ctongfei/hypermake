package forge.collection

import scala.collection._

class Alphabet private(keys: Array[String], indices: Map[String, Int]) extends IndexedSeq[String] {

  def apply(i: Int): String = keys(i)

  def index(s: String): Int = indices(s)

  def length: Int = keys.length

}
