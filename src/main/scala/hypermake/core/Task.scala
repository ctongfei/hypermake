package hypermake.core

import better.files.File

import scala.collection._
import scala.sys._
import zio._
import hypermake.collection._
import hypermake.execution.RuntimeContext
import hypermake.semantics.SymbolTable
import hypermake.util._


class Task(val name: Name,
           val env: Env,
           val `case`: Case,
           val inputs: Map[Name, Value],
           val inputEnvs: Map[Name, Env],
           val outputFileNames: Map[Name, Value],
           val outputEnvs: Map[Name, Env],
           val rawScript: Script)
          (implicit ctx: SymbolTable) extends Job()(ctx) {

  override lazy val hashCode = id.hashCode

  override def equals(o: Any) = o match {
    case that: Task => this.id == that.id
    case _ => false
  }

  override def toString = id

}

class PointedCubeTask(val name: Name,
                      val env: Env,
                      val cases: PointedCaseCube,
                      val inputs: Map[Name, PointedCube[Value]],
                      val inputEnvs: Map[Name, Env],
                      val outputNames: Map[Name, PointedCube[Value]],
                      val outputEnvs: Map[Name, Env],
                      val script: PointedCube[Script]
                     )(implicit ctx: SymbolTable)
  extends PointedCube[Task]
{ self =>

  def get(c: Case): Option[Task] = {
    if (cases containsCase c) {
      val cc = cases.normalizeCase(c)
      val is = inputs.mapValuesE(_.select(c).default)
      val os = outputNames.mapValuesE(_.select(c).default)
      val scr = script.select(c).default
      Some(new Task(name, env, cc, is, inputEnvs, os, outputEnvs, scr))
    } else None
  }
}
