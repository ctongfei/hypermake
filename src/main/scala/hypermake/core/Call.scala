package hypermake.core

import scala.collection._
import hypermake.collection._
import hypermake.semantics._
import hypermake.util._

/**
 * A call to a Hypermake [[Func]] that is used to wrap around a script.
 */
class Call(
            val args: Map[Name, Value],
            val inputScript: Name,
            val inputScriptFilename: String,
            val output: Script
          ) {

  /** Wraps around input script and returns output script. */
  def apply(input: Script): Script = {
    val r = output
      .withNewArgs(input.args ++ args)
      .withNewOutputArgs(input.outputArgs)
      .withArgs(inputScript.name -> inputScriptFilename)
    r
  }

}

class PointedCubeCall(
                       val cases: PointedCaseCube,
                       val args: Map[Name, PointedCube[Value]],
                       val inputScript: Name,
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
