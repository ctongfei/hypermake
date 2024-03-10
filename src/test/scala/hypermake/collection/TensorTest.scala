package hypermake.collection

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.{Checkers, ScalaCheckPropertyChecks}
import cats.kernel.laws.discipline._
import cats.laws.discipline._
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

object TensorGen {
  import ShapeGen._

  def pointedTensor: Gen[PointedTensor[String]] = for {
    s <- pointedShape
    a <- Gen.alphaNumStr
  } yield s.make(_ => a)

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

object TensorTest extends AnyFunSuite with ScalaCheckPropertyChecks {}
