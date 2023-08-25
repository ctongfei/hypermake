package hypermake.core

import hypermake.collection._
import hypermake.semantics.Context
import hypermake.util._

import scala.collection._

/** A task is a job that is declared by the `task` definition. It is a job that is specific to a running environment.
 */
class Task(
            val name: Name,
            val env: Env,
            val `case`: Case,
            val inputs: Map[Name, Value],
            val inputEnvs: Map[Name, Env],
            val outputFileNames: Map[Name, Value],
            val outputEnvs: Map[Name, Env],
            val decorators: Seq[Call],
            val rawScript: Script
          )(implicit ctx: Context)
  extends Job()(ctx) {}

class PointedCubeTask(
                       val name: Name,
                       val env: Env,
                       val cases: PointedCaseCube,
                       val inputs: Map[Name, PointedCube[Value]],
                       val inputEnvs: Map[Name, Env],
                       val outputNames: Map[Name, PointedCube[Value]],
                       val outputEnvs: Map[Name, Env],
                       val decorators: Seq[PointedCube[Call]],
                       val script: PointedCube[Script]
                     )(implicit ctx: Context)
  extends PointedCube[Task] {
  self =>

  def get(c: Case): Option[Task] = {
    if (cases containsCase c) {
      val cc = cases.normalizeCase(c)
      val is = inputs.mapValuesE(_.select(c).default)
      val os = outputNames.mapValuesE(_.select(c).default)
      val calls = decorators.map(_.select(c).default)
      val scr = script.select(c).default
      Some(new Task(name, env, cc, is, inputEnvs, os, outputEnvs, calls, scr))
    } else None
  }

  def dependentTaskCubes(implicit ctx: Context) = {
    val allTaskNames = inputs.values.flatMap { pcv =>
      pcv.allElements.flatMap(_.dependencies.map(_.name).toSet)
    } ++ decorators.flatMap { pcc =>
      pcc.allElements.flatMap(_.args.values.collect { case Value.Output(_, _, j) =>
        j.name
      })
    }
    allTaskNames.map(ctx.getTask)
  }

  override def equals(obj: Any) = obj match {
    case obj: PointedCubeTask => this.name == obj.name
    case _ => false
  }

  override def hashCode() = name.hashCode()

}
