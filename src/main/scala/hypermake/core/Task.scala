package hypermake.core

import scala.collection._

import cats.implicits._

import hypermake.collection._
import hypermake.util._

/** A task is the atomic unit of scripts that is executed by HyperMake. */
class Task(
    val name: String,
    val `case`: Case,
    val inputs: Args[Value],
    val inputFs: Map[String, FileSys],
    val outputFileNames: Args[Value.Pure],
    val outputFs: Map[String, FileSys],
    val decorators: Seq[Decorator],
    val rawScript: Script,
    val ephemeral: Boolean = false,
    val isPackage: Boolean = false
)(implicit val ctx: Context)
    extends Job()(ctx) {

  def outputs: Args[Value.Output] = Args(outputFileNames.keys.map { k =>
    k -> Value.Output(outputFileNames(k).value, outputFs.getOrElse(k, FileSys.local), this)
  }.toMap)
}

class PointedTaskTensor(
    val name: String,
    val shape: PointedShape,
    val inputs: PointedArgsTensor[Value],
    val inputFs: Map[String, FileSys],
    val outputFileNames: PointedArgsTensor[Value.Pure],
    val outputFs: Map[String, FileSys],
    val decorators: Seq[PointedDecoratorTensor],
    val script: PointedTensor[Script],
    val ephemeral: Boolean = false,
    val isPackage: Boolean = false
)(implicit ctx: Context)
    extends PointedTensor[Task]
    with Partial[PointedTaskTensor] {
  self =>

  def get(c: Case): Option[Task] = {
    if (shape containsCase c) {
      val cc = shape.normalizeCase(c)
      val is = inputs(c)
      val os = outputFileNames(c)
      val decs = decorators.map(_(c))
      val scr = script(c)
      Some(new Task(name, cc, is, inputFs, os, outputFs, decs, scr, ephemeral, isPackage))
    } else None
  }

  def dependentTaskTensors(implicit ctx: Context) = {
    val allTaskNames = inputs.args.values.flatMap { pcv =>
      pcv.allElements.flatMap(_.dependencies.map(_.name).toSet)
    }
    val decoratorTaskNames = decorators.flatMap { dec =>
      dec.script.allElements.flatMap(_.args.values.collect { case Value.Output(_, _, j, _) =>
        j.name
      })
    }
    (allTaskNames ++ decoratorTaskNames) map ctx.root.tasks.apply
  }

  def partial(args: PointedArgsTensor[Value]): PointedTaskTensor = {
    val outScript = script.productWith(args)(_ withNewArgs _)
    new PointedTaskTensor(
      name,
      shape,
      inputs,
      inputFs,
      outputFileNames,
      outputFs,
      decorators,
      outScript,
      ephemeral,
      isPackage
    )
  }

  override def equals(obj: Any) = obj match {
    case obj: PointedTaskTensor => this.name == obj.name
    case _                      => false
  }

  override def hashCode() = name.hashCode()

}

object PointedTaskTensor {
  implicit val ordering: Ordering[PointedTaskTensor] = Ordering.by(_.name)
}
