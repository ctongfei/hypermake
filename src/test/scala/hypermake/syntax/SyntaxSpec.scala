package hypermake.syntax

import org.scalacheck._
import hypermake.syntax.ast._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

object SyntaxGen {

  def identifier = Gen.identifier.map(Identifier)
  def identifierPath = Gen.nonEmptyListOf(identifier).map(IdentifierPath)
  def envModifier = Gen.option(identifierPath).map(EnvModifier)
  def stringLiteral = for {
    s <- Gen.alphaNumStr
    m <- envModifier
  } yield StringLiteral(s, m)
  def axisName = Gen.alphaNumStr.map(AxisName.apply)

  def dictLiteral: Gen[DictLiteral] = ???

  def literal: Gen[Literal] = stringLiteral

}

class SyntaxSpecTest extends AnyFunSuite with ScalaCheckPropertyChecks {

  test("Literal serde") {
    forAll(SyntaxGen.literal) { a =>
      println(a)
      val str = a.toString
      assert(a == parseFor(Expressions.literal(_))(str).get.value)
    }
  }
}
