package hypermake

import scala.collection.immutable.{Map => IMap}
import scala.collection._
import fastparse._
import hypermake.collection._
import hypermake.exception._
import zio._
import zio.stream.ZSink


package object util {

  type HIO[+A] = ZIO[ZEnv, Throwable, A]
  type HSink[A] = ZSink[ZEnv, Throwable, A, A, Long]

  private def mapViewAsMap[A, B](m: MapView[A, B]): IMap[A, B] = new DefaultMapBase[A, B] {
    override def get(key: A) = m.get(key)
    override def iterator = m.iterator
  }

  implicit class SetExtension[A](val s: Set[A]) {
    def makeMap[B](f: A => B): Map[A, B] = new DefaultMapBase[A, B] {
      def get(key: A) = if (s.contains(key)) Some(f(key)) else None
      def iterator = s.iterator.map(a => a -> f(a))
    }
  }

  implicit class MapExtensions[A, B](val m: Map[A, B]) {
    def filterKeysE(f: A => Boolean): IMap[A, B] = m.view.filterKeys(f).toMap
    def filterKeysL(f: A => Boolean): IMap[A, B] = mapViewAsMap(m.view.filterKeys(f))
    def mapValuesE[C](f: B => C): IMap[A, C] = m.view.mapValues(f).toMap
    def mapValuesL[C](f: B => C): IMap[A, C] = mapViewAsMap(m.view.mapValues(f))
    def pointed(defaultKey: A): PointedMap[A, B] = PointedMap(m, defaultKey)
  }

  implicit class StringPathJoin(val s: String) extends AnyVal {
    def /(t: String) = s + java.io.File.separator + t
  }

  def orderedSet[A](elems: Iterable[A]): Set[A] =
    scala.collection.mutable.LinkedHashSet.from(elems)  // maintains order in the keys

  def orderedMap[A, B](pairs: Iterable[(A, B)]): Map[A, B] =
    scala.collection.mutable.LinkedHashMap.from(pairs)  // maintains order in the keys

}
