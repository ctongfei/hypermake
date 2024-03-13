package hypermake.syntax

import cats.implicits.toTraverseOps
import org.scalacheck._
import org.scalacheck.cats.implicits._
import hypermake.collection._
import hypermake.syntax.ast._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

object ExprGen {
  import ShapeGen._

  def identifier = Gen.identifier.map(Identifier)
  def identifierPath = Gen.nonEmptyListOf(identifier).map(IdentifierPath)
  def envModifier = Gen.option(identifierPath).map(FileSysModifier)
  def stringLiteral = for {
    s <- Gen.alphaNumStr
    m <- envModifier
  } yield StringLiteral(s, m)
  def axisName = Gen.identifier.map(AxisName.apply)

  def shapedDictLiteral(shape: Shape): Gen[DictLiteral] = for {
    axis <- Gen.oneOf(shape.vars.toSeq)
    elems <-
      if (shape.vars.size > 1)
        Gen.listOfN(shape(axis).size, shapedDictLiteral(shape.filterVars(_ != axis)))
      else Gen.listOfN(shape(axis).size, stringLiteral)
  } yield DictLiteral(AxisName(axis.name), Map.from(shape(axis) zip elems))

  def dictLiteral: Gen[DictLiteral] = for {
    s <- pointedShape.filter(_.vars.nonEmpty)
    d <- shapedDictLiteral(s)
  } yield d

  def literal: Gen[Literal] = Gen.oneOf(stringLiteral, dictLiteral)

  def axisIndices(shape: PointedShape): Gen[AxisIndices] = for {
    axes <- Gen.someOf(shape.vars)
    keys <- axes.toSeq.traverse(a => pointedSubset(shape(a)))
  } yield {
    AxisIndices((axes zip keys).map { case (a, ks) => AxisIndex(Identifier(a.name), Keys(ks)) })
  }

  def outputRef = identifier.map(OutputRef)

  def valRef: Gen[ValRef] = for {
    n <- identifierPath
    s <- pointedShape
    i <- axisIndices(s)
    o <- Gen.option(outputRef)
  } yield ValRef(n, i, o)

  def expr: Gen[Expr] = Gen.oneOf(literal, valRef)

  implicit val arbLiteral: Arbitrary[Literal] = Arbitrary(literal)
  implicit val arbExpr: Arbitrary[Expr] = Arbitrary(expr)
}

class ExprSyntaxTest extends AnyFunSuite with FunSuiteDiscipline with ScalaCheckPropertyChecks {
  import ExprGen._
  checkAll(
    "ExprSyntaxInvertibility",
    new SyntaxInvertibility[Expr](Expressions.expr(_)).invertibility
  )
}
