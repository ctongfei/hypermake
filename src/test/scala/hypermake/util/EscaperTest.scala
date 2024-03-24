package hypermake.util

import org.scalacheck._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

object StringGen {

  // stress test
  val char = Gen.oneOf(' ', '\'', '\"', '\\', '\n', '\t', '\r', '\f', '\b', 'a', 'A', '0')
  val str = Gen.stringOf(char)
  implicit val arbString: Arbitrary[String] = Arbitrary(str)

}

class EscaperInvertibility(val escaper: Escaper) extends Laws {
  import StringGen._
  def invertibility = new DefaultRuleSet(
    "invertibility",
    None,
    "escape" -> Prop.forAll((s: String) => {
      escaper.unescape(escaper.escape(s)) == s
    })
  )
}

class EscaperTest extends AnyFunSuite with FunSuiteDiscipline with ScalaCheckPropertyChecks {

  checkAll("Escaper.Shell", new EscaperInvertibility(Escaper.Shell).invertibility)
  checkAll("Escaper.C", new EscaperInvertibility(Escaper.C).invertibility)
  checkAll("Escaper.Percent", new EscaperInvertibility(Escaper.Percent).invertibility)

}
