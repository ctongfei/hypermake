package hypermake.core

import scala.collection._

import cats.implicits._

import hypermake.collection._
import hypermake.semantics.Context
import hypermake.util._

/** A task is a job that is declared by the `task` definition. It is a job that is specific to a
  * running environment.
  */
class Task(
    val name: String,
    val fileSys: FileSys,
    val `case`: Case,
    val inputs: Map[String, Value],
    val inputFs: Map[String, FileSys],
    val outputFileNames: Map[String, Value.Pure],
    val outputFs: Map[String, FileSys],
    val decorators: Seq[Decorator],
    val rawScript: Script
)(implicit ctx: Context)
    extends Job()(ctx) {

  def outputs: Map[String, Value.Output] = outputFileNames.keys.map { k =>
    k -> Value.Output(outputFileNames(k).value, outputFs.getOrElse(k, fileSys), this)
  }.toMap
}

class PointedTaskTensor(
    val name: String,
    val fs: FileSys,
    val shape: PointedShape,
    val inputs: Map[String, PointedTensor[Value]],
    val inputFs: Map[String, FileSys],
    val outputFileNames: Map[String, PointedTensor[Value.Pure]],
    val outputFs: Map[String, FileSys],
    val decorators: Seq[Decorator],
    val script: PointedTensor[Script]
)(implicit ctx: Context)
    extends PointedTensor[Task] {
  self =>

  def get(c: Case): Option[Task] = {
    if (shape containsCase c) {
      val cc = shape.normalizeCase(c)
      val is = inputs.mapValuesE(_.select(c).default)
      val os = outputFileNames.mapValuesE(_.select(c).default)
      val scr = script.select(c).default
      Some(new Task(name, fs, cc, is, inputFs, os, outputFs, decorators, scr))
    } else None
  }

  def dependentTaskCubes(implicit ctx: Context) = {
    val allTaskNames = inputs.values.flatMap { pcv =>
      pcv.allElements.flatMap(_.dependencies.map(_.name).toSet)
    }
    val decoratorTaskNames = decorators.flatMap { dec =>
      dec.script.allElements.flatMap(_.args.values.collect { case Value.Output(_, _, j) =>
        j.name
      })
    }
    (allTaskNames ++ decoratorTaskNames).map(k => ctx.root.tasks(k))
  }

  def withNewArgs(args: Map[String, PointedTensor[Value]]): PointedTaskTensor = {
    val outScript = script.productWith(args.toMap.unorderedSequence)(_ withNewArgs _)
    new PointedTaskTensor(
      name,
      fs,
      shape,
      inputs,
      inputFs,
      outputFileNames,
      outputFs,
      decorators,
      outScript
    )
  }

  override def equals(obj: Any) = obj match {
    case obj: PointedTaskTensor => this.name == obj.name
    case _                      => false
  }

  override def hashCode() = name.hashCode()

}
