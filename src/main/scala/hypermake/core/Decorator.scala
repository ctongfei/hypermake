package hypermake.core

import scala.collection._

import hypermake.collection._
import hypermake.exception.ObjectIsNotDecoratorException
import hypermake.semantics._
import hypermake.util._

/** An object that takes the signature `def run(inputScript): outputScript` and wraps around the
  * input script to produce the output script. This is used to decorate tasks.
  */
case class Decorator(
    innerFileArg: String,
    script: PointedTensor[Script],
    alive: Option[PointedTensor[Script]]
) {

  /** Wraps around input script and returns output script.
    *
    * @param input
    *   input script to be wrapped
    * @param fs
    *   file system of the underlying running task
    */
  def apply(input: PointedTensor[Script], fs: FileSys): PointedTensor[Script] =
    (script productWith input) { case (scr, inp) =>
      Script(
        scr.script,
        inp.args ++ scr.args ++ Map(innerFileArg -> Value.Input(f"script.${inp.nestingLevel}", fs)),
        inp.nestingLevel + 1
      )
    }

}

object Decorator {
  def fromObj(obj: Obj): Decorator = {
    val runFunc = obj.funcTable.getOrElse("run", throw ObjectIsNotDecoratorException(obj))
    if (runFunc.params.size != 1) throw ObjectIsNotDecoratorException(obj)

    // TODO: alive
    val aliveFunc = obj.funcTable.get("alive")
    if (aliveFunc.nonEmpty && aliveFunc.get.params.nonEmpty)
      throw ObjectIsNotDecoratorException(obj)

    Decorator(
      runFunc.params.head,
      runFunc.impl,
      aliveFunc.map(_.impl)
    )
  }
}
