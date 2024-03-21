package hypermake.util

import scala.collection._

trait MapWrapper[K, +V] extends DefaultMapBase[K, V] {
  def underlying: Map[K, V]

  def get(key: K) = underlying.get(key)

  def iterator = underlying.iterator
}
