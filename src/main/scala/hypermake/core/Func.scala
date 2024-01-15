package hypermake.core

import cats.instances.map._
import cats.syntax.unorderedTraverse._
import hypermake.collection._
import hypermake.semantics.Context
import hypermake.util._

import scala.collection._

/** Defines a Hypermake function. A function takes in a script, optionally a set of parameters, and outputs a script. A
  * function can be directly used as an implementation or a task, or can be used as a decorator to wrap around a task.
  */
case class Func(
    name: String,
    params: Map[String, PointedCube[Value]],
    inputScript: String,
    inputScriptFilename: String,
    impl: PointedCube[Script]
) {

  /** Constructs the complete script with the unbound variables assigned.
    */
  def reify(args: Map[String, PointedCube[Value]])(implicit ctx: Context) = {
    val reified = withNewArgs(args)
    val unboundedParams = reified.params
    val axes = args.values.map(_.cases.vars).fold(Set())(_ union _)
    new PointedCubeCall(
      ctx.allCases.filterVars(axes),
      args ++ unboundedParams,
      inputScript,
      inputScriptFilename,
      reified.impl
    )
  }

  /** Fills in some of the parameters of this function (currying).
    */
  def withNewArgs(args: Map[String, PointedCube[Value]]): Func = {
    val unboundParams = params.filterKeysE(a => !args.contains(a))
    val outScript = impl.productWith(args.toMap.unorderedSequence)(_ withNewArgs _)
    Func(name, unboundParams, inputScript, inputScriptFilename, outScript)
  }

}
