package hypermake.core

import hypermake.collection._
import hypermake.syntax.ast.Identifier

object ParameterizedTest extends App {

  import PointedCube._

  val c = of(
    "lang",
    "en" -> of("side", "src" -> Singleton(0), "tgt" -> Singleton(1)),
    "zh" -> of("side", "src" -> Singleton(2), "tgt" -> Singleton(3)),
    "es" -> of("side", "src" -> Singleton(4), "tgt" -> Singleton(5)),
  )

  val d = of(
    "dropout",
    "0.1" -> Singleton("0.1"),
    "0.2" -> Singleton("0.2"),
    "0.3" -> Singleton("0.3"),
  )

  val e = for {
    a <- c
    b <- d
  } yield (a, b)

  for ((i, x) <- e.allPairs)
    println((i, x))

}
