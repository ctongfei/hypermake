package hypermake.util

import scala.collection._


trait MapWrapper[K, +V] extends Map[K, V] {
  def underlying: Map[K, V]

  def -(key: K) = underlying - key
  def -(key1: K, key2: K, keys: K*) = underlying.-(key1, key2, keys: _*)

  def get(key: K) = underlying.get(key)
  def iterator = underlying.iterator
}
