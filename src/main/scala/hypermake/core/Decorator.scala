package hypermake.core

import hypermake.collection._
import hypermake.exception.ObjectIsNotDecoratorException
import hypermake.semantics._
import hypermake.util._

import scala.collection._

/** An object that takes the signature `def apply(inputScript): outputScript` and wraps around the input script to
  * produce the output script.
  */
class Decorator(
    val innerFileArg: String,
    val script: Script
) {

  /** Wraps around input script and returns output script.
    * @param input
    *   input script to be wrapped
    * @param env
    *   environment of the underlying running task
    */
  def apply(input: Script, env: Env): Script = Script(
    script.script,
    input.args ++ script.args ++ Map(innerFileArg -> Value.Input(f"script.${input.nestingLevel}", env)),
    input.nestingLevel + 1
  )
}

object Decorator {
  def fromObj(obj: Obj)(implicit ctx: Context) = {
    val applyFunc = obj.funcTable.getOrElse("apply", throw new ObjectIsNotDecoratorException(obj))
    if (applyFunc.params.size != 1) throw ObjectIsNotDecoratorException(obj)
    new Decorator(
      applyFunc.params.head,
      applyFunc.impl
    )
  }
}

//
//class PointedTensorDecorator(
//                              val cases: PointedCaseTensor,
//                              val args: Map[String, PointedTensor[Value]],
//                              val inner: String,
//                              val innerFilename: String,
//                              val script: PointedTensor[Script]
//)(implicit ctx: Context)
//    extends PointedTensor[Decorator] {
//  self =>
//
//  def get(c: Case): Option[Decorator] = {
//    if (cases containsCase c) {
//      val as = args.mapValuesE(_.select(c).default)
//      val scr = script.select(c).default.withNewArgs(as).withArgs(inner -> innerFilename)
//      Some(new Decorator(inner, innerFilename, scr))
//    } else None
//  }
//}

//object PointedTensorDecorator {
//  def fromObj(obj: Obj)(implicit ctx: Context) = {
//    val applyFunc = obj.funcTable.getOrElse("apply", throw new ObjectIsNotDecoratorException(obj))
//    if (applyFunc.params.size != 1) throw new ObjectIsNotDecoratorException(obj)
//    new PointedTensorDecorator(
//      applyFunc.impl.cases,
//      Map(),
//      applyFunc.params.head._1,
//      applyFunc.params.head._2.default.value,
//      applyFunc.impl
//    )
//  }
//}
