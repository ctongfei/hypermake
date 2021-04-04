package hypermake

import scala.collection.immutable.{Map => IMap}
import scala.collection._
import fastparse._
import hypermake.core._
import hypermake.exception._

import scala.util.{Failure, Success, Try}


package object util {

  implicit class ParsedExtensions[A](val p: Parsed[A]) {

    def asTry: Try[A] = p match {
      case Parsed.Success(a, _) => Success(a)
      case f @ Parsed.Failure(_, _, _) => Failure(ParsingException(f))
    }
  }

  implicit class MapExtensions[A, B](val m: Map[A, B]) {
    def filterKeysE(f: A => Boolean): Map[A, B] = m.view.filterKeys(f).toMap
    def mapValuesE[C](f: B => C): IMap[A, C] = m.view.mapValues(f).toMap
    def pointed(defaultKey: A): PointedMap[A, B] = PointedMap(m, defaultKey)
  }

  implicit class StringPathJoin(val s: String) extends AnyVal {
    def /(t: String) = s + java.io.File.separator + t
  }

}
