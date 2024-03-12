package hypermake.collection

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.{Checkers, ScalaCheckPropertyChecks}
import cats.kernel.laws.discipline._
import cats.laws.discipline._
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import hypermake.util._

object TensorGen {
  import ShapeGen._

  def pointedTensor: Gen[PointedTensor[String]] = for {
    s <- pointedShape
    a <- Gen.listOfN(s.numElements, Gen.alphaNumStr)
  } yield new PointedTensor[String] {
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
      Some(a(index))
    }
  }

  def pointedTensorF: Gen[PointedTensor[String => String]] =
    pointedShape.map(_.make(_ => identity))

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
