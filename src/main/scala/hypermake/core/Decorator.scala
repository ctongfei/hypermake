package hypermake.core

import scala.collection._

import hypermake.collection._
import hypermake.exception.ObjectIsNotDecoratorException
import hypermake.semantics._
import hypermake.util._

/**
 * Represents a decorator, which modifies a [[Script]].
 * @param innerFileArg The name of the argument that holds the inner script.
 * @param script The script that wraps around the inner script.
 */
case class Decorator(
    innerFileArg: String,
    script: Script
) {
  def apply(input: Script, fs: FileSys): Script =
    Script(
      script.script,
      input.args ++ script.args ++ Map(
        innerFileArg -> Value.Input(s"script.${input.nestingLevel}", fs)
      ),
      input.nestingLevel + 1
    )

}

/**
 * An object that takes the signature `def run(inputScript): outputScript`
 * and wraps around the input script to produce the output script.
 * This is used to decorate tasks.
 */
// TODO: do we add `alive` functions to submitters?
case class PointedDecoratorTensor(
    innerFileArg: String,
    script: PointedTensor[Script]
    // alive: Option[PointedTensor[Script]]
) extends PointedTensor[Decorator] {

  def shape: PointedShape = script.shape

  def get(c: Case): Option[Decorator] = {
    script.get(c).map(Decorator(innerFileArg, _))
  }

  /**
   * Wraps around input script and returns output script.
   * @param input input script to be wrapped
   * @param fs file system of the underlying running task
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

object PointedDecoratorTensor {
  def fromObj(obj: Obj): PointedDecoratorTensor = {
    val runFunc = obj.funcTable.getOrElse("run", throw ObjectIsNotDecoratorException(obj))
    if (runFunc.params.size != 1) throw ObjectIsNotDecoratorException(obj)

    // TODO: alive
//    val aliveFunc = obj.funcTable.get("alive")
//    if (aliveFunc.nonEmpty && aliveFunc.get.params.nonEmpty)
//      throw ObjectIsNotDecoratorException(obj)

    PointedDecoratorTensor(
      runFunc.params.head,
      runFunc.impl
      // aliveFunc.map(_.impl)
    )
  }
}
