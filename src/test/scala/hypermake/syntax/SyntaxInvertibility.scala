package hypermake.syntax

import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

class SyntaxInvertibility[A](parser: fastparse.P[_] => fastparse.P[A])(implicit arbA: Arbitrary[A])
    extends Laws {

  def invertibility = new DefaultRuleSet(
    "invertibility",
    None,
    "same" -> Prop.forAll((a: A) => parseFor(parser)(a.toString).get.value == a)
  )
}
