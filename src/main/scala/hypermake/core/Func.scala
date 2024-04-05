package hypermake.core

import scala.collection._

import cats.implicits.catsStdInstancesForMap
import cats.syntax.unorderedTraverse._

import hypermake.collection._

/** Encapsulates a reusable snippet of code across tasks. */
case class Func(
    name: String,
    inputs: Params[Value],
    outputs: Params[Value],
    impl: PointedTensor[Script]
) extends Partial[Func] { self =>

  def shape = impl.shape

  def partial(args: PointedArgsTensor[Value]): Func = {
    val outScript = impl.productWith(args)(_ withNewArgs _)
    new Func(name, inputs bind args, outputs, outScript)
  }

  def reify: PointedTensor[Script] = {
    assert(!outputs.hasUnboundVars)
    impl.productWith(inputs.toArgsIfBound(Some(name)))(_ withNewArgs _) // bind the default values
  }

}
