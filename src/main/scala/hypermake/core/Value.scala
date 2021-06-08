package hypermake.core

import hypermake.collection._
import hypermake.execution._

import scala.collection._

/**
 * This is essentially a string potentially paired with multiple dependent tasks.
 */
sealed trait Value {
  def value: String
  def isPath: Boolean = envOption.isDefined
  def envOption: Option[Env]
  def dependencies: Set[Job]
  override def toString = value
}

object Value {

  case class Pure(value: String) extends Value {
    def envOption = None
    def dependencies = Set()
  }

  sealed trait Path extends Value {
    def env: Env
    override def envOption = Some(env)
  }

  case class Input(value: String, env: Env) extends Path {
    def dependencies = Set()
  }

  case class Output(value: String, env: Env, job: Job) extends Path {
    def dependencies = Set(job)
  }

  case class Multiple(cases: Cube[Value], env: Env)(implicit runtime: RuntimeContext) extends Path {
    override def value = cases.map(_.value).allElements.mkString(runtime.IFS_CHAR)
    override def dependencies = cases.map(_.dependencies).allElements.reduce(_ union _)
  }

}
