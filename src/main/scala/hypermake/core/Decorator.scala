package hypermake.core

import hypermake.collection._
import hypermake.exception.ObjectIsNotDecoratorException
import hypermake.semantics._
import hypermake.util._

import scala.collection._

/** An object that takes the signature `def apply(inputScript): outputScript` and wraps around the input script to
  * produce the output script. This is used to decorate tasks.
  */
case class Decorator(
    innerFileArg: String,
    script: PointedTensor[Script],
    alive: Option[PointedTensor[Script]]
) {

  /** Wraps around input script and returns output script.
   *
   * @param input
   * input script to be wrapped
   * @param env
   * environment of the underlying running task
   */
  def apply(input: PointedTensor[Script], env: Env): PointedTensor[Script] = (script productWith input) {
    case (scr, inp) =>
      Script(
        scr.script,
        inp.args ++ scr.args ++ Map(innerFileArg -> Value.Input(f"script.${inp.nestingLevel}", env)),
        inp.nestingLevel + 1
      )
    }

}

object Decorator {
  def fromObj(obj: Obj): Decorator = {
    val applyFunc = obj.funcTable.getOrElse("apply", throw ObjectIsNotDecoratorException(obj))
    if (applyFunc.params.size != 1) throw ObjectIsNotDecoratorException(obj)

    val aliveFunc = obj.funcTable.get("alive")
    if (aliveFunc.nonEmpty && aliveFunc.get.params.nonEmpty) throw ObjectIsNotDecoratorException(obj)

    Decorator(
      applyFunc.params.head,
      applyFunc.impl,
      aliveFunc.map(_.impl)
    )
  }
}
