package hypermake.core

import hypermake.collection._
import hypermake.semantics._
import hypermake.util._

import scala.collection._

/** A call to a Hypermake [[Func]] that is used to wrap around a script.
  */
class Call(
    val args: Map[String, Value],
    val inputScript: String,
    val inputScriptFilename: String,
    val output: Script
) {

  /** Wraps around input script and returns output script. */
  def apply(input: Script): Script = {
    val r = output
      .withNewArgs(input.args ++ args)
      .withNewOutputArgs(input.outputArgs)
      .withArgs(inputScript -> inputScriptFilename)
    r
  }

}

class PointedCubeCall(
    val cases: PointedCaseCube,
    val args: Map[String, PointedCube[Value]],
    val inputScript: String,
    val inputScriptFilename: String,
    val output: PointedCube[Script]
)(implicit ctx: Context)
    extends PointedCube[Call] {
  self =>

  def get(c: Case): Option[Call] = {
    if (cases containsCase c) {
      val as = args.mapValuesE(_.select(c).default)
      val scr = output.select(c).default
      Some(new Call(as, inputScript, inputScriptFilename, scr))
    } else None
  }
}
