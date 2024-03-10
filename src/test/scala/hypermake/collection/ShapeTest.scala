package hypermake.collection

import scala.collection.mutable

import cats.kernel.laws.discipline._
import cats.kernel.PartialOrder
import org.scalacheck.{Arbitrary, Cogen, Gen, Prop}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.{Checkers, ScalaCheckPropertyChecks}
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import org.typelevel.discipline.Laws

object ShapeGen {

  val axes: mutable.HashMap[Axis, PointedSet[String]] = mutable.HashMap()

  def axisLabel: Gen[Axis] = Gen.identifier.map(Axis.apply)

  def nonEmptySet: Gen[Set[String]] = for {
    n <- Gen.poisson(5).filter(_ > 0)
    s <- Gen.listOfN(n, Gen.alphaNumStr)
  } yield s.toSet

  def pointedSet: Gen[PointedSet[String]] = for {
    s <- nonEmptySet
    d <- Gen.oneOf(s.toSeq)
  } yield PointedSet(s, d)

  def axis: Gen[(Axis, PointedSet[String])] = for {
    a <- axisLabel
    s <- pointedSet
  } yield {
    if (axes contains a) (a, axes(a)) // retrieves the cached axis
    else {
      axes(a) = s
      (a, s)
    }
  }

  def pointedShape: Gen[PointedShape] = for {
    n <- Gen.poisson(3)
    axes <- Gen.listOfN(n, axis)
  } yield PointedShape(axes.toMap)

  implicit val cogenPointedShape: Cogen[PointedShape] =
    Cogen[PointedShape]((x: PointedShape) => x.hashCode.toLong)
  implicit val arbPointedShape: Arbitrary[PointedShape] = Arbitrary(pointedShape)

}

class PartialOrdersAgree[A](val x: PartialOrder[A], val y: PartialOrder[A])(implicit
    arbA: Arbitrary[A]
) extends Laws {
  def agree = new DefaultRuleSet(
    "partialOrdersAgree",
    None,
    "same" -> Prop.forAll((a: A, b: A) => x.lteqv(a, b) == y.lteqv(a, b))
  )
}

class ShapeTest
    extends AnyFunSuite
    with FunSuiteDiscipline
    with ScalaCheckPropertyChecks
    with Checkers {
  import ShapeGen._

  checkAll(
    "PointedShape BoundedSemilattice laws",
    BoundedSemilatticeTests[PointedShape].boundedSemilattice
  )

  checkAll(
    "PointedShape PartialOrder laws",
    PartialOrderTests[PointedShape].partialOrder
  )

  checkAll(
    "PointedShape BoundedSemilattice agrees with PartialOrder",
    new PartialOrdersAgree(
      PointedShape.PartialOrder,
      PointedShape.BoundedSemilattice.asJoinPartialOrder
    ).agree
  )

}
