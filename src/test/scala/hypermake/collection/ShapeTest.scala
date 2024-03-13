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
    n <- Gen.choose(1, 4)
    s <- Gen.listOfN(n, Gen.alphaNumStr.filter(_.nonEmpty))
  } yield s.toSet

  def pointedSet: Gen[PointedSet[String]] = for {
    s <- nonEmptySet
    d <- Gen.oneOf(s.toSeq)
  } yield PointedSet(s, d)

  def pointedSubset[A](s: PointedSet[A]): Gen[PointedSet[A]] = for {
    elems <- Gen.someOf(s)
  } yield PointedSet(Set.from(elems :+ s.default), s.default)

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
    n <- Gen.choose(0, 5)
    axes <- Gen.listOfN(n, axis)
  } yield PointedShape(axes.toMap)

  implicit val cogenPointedShape: Cogen[PointedShape] =
    Cogen[PointedShape]((x: PointedShape) => x.hashCode.toLong)
  implicit val arbPointedShape: Arbitrary[PointedShape] = Arbitrary(pointedShape)

}

class PartialOrdersConsistency[A](val x: PartialOrder[A], val y: PartialOrder[A])(implicit
    arbA: Arbitrary[A]
) extends Laws {
  def consistency = new DefaultRuleSet(
    "partialOrdersConsistency",
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
    "PointedShape BoundedSemilattice consistent with PartialOrder",
    new PartialOrdersConsistency(
      PointedShape.PartialOrder,
      PointedShape.BoundedSemilattice.asJoinPartialOrder
    ).consistency
  )

}
