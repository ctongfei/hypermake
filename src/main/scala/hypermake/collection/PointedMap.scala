package hypermake.collection

import scala.collection._

/** A map with a default key. */
trait PointedMap[K, +V] extends Map[K, V] {
  self =>

  /** The default key in this map. */
  def defaultKey: K

  override def keySet: PointedSet[K] = PointedSet(super.keySet, defaultKey)

  def defaultPair: (K, V) = (defaultKey, this(defaultKey))

  def defaultValue: V = this(defaultKey)

  def underlying: Map[K, V]

  def -(key: K) = underlying - key

  def -(key1: K, key2: K, keys: K*) = underlying.-(key1, key2, keys: _*)

  def get(key: K) = underlying.get(key)

  def iterator = underlying.iterator
}

object PointedMap {

  def apply[K, V](underlyingMap: Map[K, V], defaultKey0: K): PointedMap[K, V] = {
    assert(underlyingMap contains defaultKey0)
    new PointedMap[K, V] {
      def defaultKey = defaultKey0

      def underlying = underlyingMap
    }
  }

}
