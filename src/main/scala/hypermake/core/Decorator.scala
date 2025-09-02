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
      input.args ++ script.args ++ Args(
        Map(
          // relative path to the working directory, not to the output root
          innerFileArg -> Value.Pure(s"script.${input.nestingLevel}")
        )
      ),
      input.nestingLevel + 1
    )

}

/**
 * An object that takes the signature `def run(inputScript): outputScript`
 * and wraps around the input script to produce the output script.
 * This is used to decorate tasks.
 */
case class PointedDecoratorTensor(
    innerFileArg: String,
    script: PointedTensor[Script]
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
        inp.args ++ scr.args ++ Args(Map(innerFileArg -> Value.Input(f"script.${inp.nestingLevel}", fs))),
        inp.nestingLevel + 1
      )
    }

}

object PointedDecoratorTensor {
  def fromObj(obj: Obj): PointedDecoratorTensor = {
    val runFunc = obj.funcTable.getOrElse("run", throw ObjectIsNotDecoratorException(obj))
    if (runFunc.inputs.params.size != 1) throw ObjectIsNotDecoratorException(obj)

    PointedDecoratorTensor(
      runFunc.inputs.params.head._1,
      runFunc.impl
    )
  }
}
