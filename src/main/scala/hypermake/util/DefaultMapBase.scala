package hypermake.util

import scala.collection._

/**
 * Endows a map with default `removed` and `updated` methods to ease the implementation of a new map.
 */
abstract class DefaultMapBase[A, +B] extends immutable.Map[A, B] {
  override def removed(key: A) = view.filterKeys(_ != key).toMap

  override def updated[V1 >: B](key: A, value: V1) = view.toMap.updated(key, value)
}
