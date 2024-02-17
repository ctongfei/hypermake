package hypermake.core

import cats.implicits._
import hypermake.collection._
import hypermake.semantics.Context
import hypermake.util._

import scala.collection._

/** A task is a job that is declared by the `task` definition. It is a job that is specific to a running environment.
  */
class Task(
    val name: String,
    val env: Env,
    val `case`: Case,
    val inputs: Map[String, Value],
    val inputEnvs: Map[String, Env],
    val outputFileNames: Map[String, Value.Pure],
    val outputEnvs: Map[String, Env],
    val decorators: Seq[Decorator],
    val rawScript: Script
)(implicit ctx: Context)
    extends Job()(ctx) {

  def outputs: Map[String, Value.Output] = outputFileNames.keys.map { k =>
    k -> Value.Output(outputFileNames(k).value, outputEnvs.getOrElse(k, env), this)
  }.toMap
}

class PointedTaskTensor(
    val name: String,
    val env: Env,
    val cases: PointedCaseTensor,
    val inputs: Map[String, PointedTensor[Value]],
    val inputEnvs: Map[String, Env],
    val outputFileNames: Map[String, PointedTensor[Value.Pure]],
    val outputEnvs: Map[String, Env],
    val decorators: Seq[Decorator],
    val script: PointedTensor[Script]
)(implicit ctx: Context)
    extends PointedTensor[Task] {
  self =>

  def get(c: Case): Option[Task] = {
    if (cases containsCase c) {
      val cc = cases.normalizeCase(c)
      val is = inputs.mapValuesE(_.select(c).default)
      val os = outputFileNames.mapValuesE(_.select(c).default)
      val scr = script.select(c).default
      Some(new Task(name, env, cc, is, inputEnvs, os, outputEnvs, decorators, scr))
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
    new PointedTaskTensor(name, env, cases, inputs, inputEnvs, outputFileNames, outputEnvs, decorators, outScript)
  }

  override def equals(obj: Any) = obj match {
    case obj: PointedTaskTensor => this.name == obj.name
    case _                      => false
  }

  override def hashCode() = name.hashCode()

}
