package hypermake.util

import hypermake.core._
import hypermake.syntax.Identifier

import scala.collection._

object Util {

  def orderedSet[A](elems: Iterable[A]): Set[A] =
    scala.collection.mutable.LinkedHashSet.from(elems)  // maintains order in the keys

  def orderedMap[A, B](pairs: Iterable[(A, B)]): Map[A, B] =
    scala.collection.mutable.LinkedHashMap.from(pairs)  // maintains order in the keys

  def joinCases(c0: Map[Identifier, Set[String]], c1: Map[Identifier, Set[String]]): Map[Identifier, Set[String]] = {
    val allKeys = c0.keySet union c1.keySet
    val joined = allKeys.view.map { (a: Identifier) =>  // a full outer join here
      if (c0.contains(a) && c1.contains(a))
        a -> (c0(a) intersect c1(a))
      else if (c0.contains(a))
        a -> c0(a)
      else
        a -> c1(a)
    }.toMap
    joined
  }

  def joinPointedCases(c0: Map[Identifier, PointedSet[String]], c1: Map[Identifier, PointedSet[String]]): Map[Identifier, PointedSet[String]] = {
    val allKeys = c0.keySet union c1.keySet
    val joined = allKeys.view.map { (a: Identifier) =>  // a full outer join here
      if (c0.contains(a) && c1.contains(a))
        a -> (c0(a) intersect c1(a))
      else if (c0.contains(a))
        a -> c0(a)
      else
        a -> c1(a)
    }.toMap
    joined
  }


}
