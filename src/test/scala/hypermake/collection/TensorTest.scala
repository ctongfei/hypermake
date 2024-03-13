package hypermake.collection

import cats.laws.discipline._
import hypermake.util._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

object TensorGen {
  import ShapeGen._

  private def genTensor[A](s: PointedShape, xs: Iterable[A]): PointedTensor[A] =
    new PointedTensor[A] {
      private[this] val elems = xs.toIndexedSeq
      private[this] val axes = s.vars.toArray
      private[this] val axisIds = axes.zipWithIndex.toMap
      private[this] val axisStrides = axes.scanLeft(1)(_ * s(_).size).init
      private[this] val keyIds = s.underlying.mapValuesE(s => s.zipWithIndex.toMap)
      def shape = s
      def get(c: Case) = {
        val index = c.underlying
          .filterKeysE(s.containsAxis)
          .map { case (k, v) =>
            keyIds(k)(v) * axisStrides(axisIds(k))
          }
          .sum
        Some(elems(index))
      }
    }

  def pointedTensor: Gen[PointedTensor[String]] = for {
    s <- pointedShape
    a <- Gen.listOfN(s.numElements, Gen.alphaNumStr)
  } yield genTensor(s, a)

  def pointedTensorF: Gen[PointedTensor[String => String]] = for {
    s <- pointedShape
    a <- Gen.listOfN(s.numElements, Gen.function1[String, String](Gen.alphaNumStr))
  } yield genTensor(s, a)

  implicit val arbPointedTensor = Arbitrary(pointedTensor)
  implicit val arbPointedTensorF = Arbitrary(pointedTensorF)
}

class TensorTest extends AnyFunSuite with FunSuiteDiscipline with Checkers {
  import TensorGen._

  checkAll(
    "PointedTensor CommutativeMonad laws",
    CommutativeMonadTests[PointedTensor].stackUnsafeMonad[String, String, String]
  )

}
