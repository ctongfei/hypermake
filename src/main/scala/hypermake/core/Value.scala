package hypermake.core

import hypermake.collection._
import hypermake.execution._

import scala.collection._

/**
 * This is essentially a string potentially paired with multiple dependent tasks.
 */
sealed trait Value {
  def value: String
  def absValue(implicit runtime: RuntimeContext): String
  def isPath: Boolean = envOption.isDefined
  def envOption: Option[Env]
  def dependencies: Set[Job]
  override def toString = value
}

object Value {

  sealed trait EnvAgnostic extends Value

  case class Pure(value: String) extends EnvAgnostic {
    def absValue(implicit runtime: RuntimeContext) = value
    def envOption = None
    def dependencies = Set()
  }

  case class PackageOutput(pack: Package) extends EnvAgnostic {
    def value = "package"
    def absValue(implicit runtime: RuntimeContext) = ???
    def envOption = None
    def dependencies = Set()
    def on(env: Env): Output = Output(value, env, pack.on(env))
  }

  sealed trait EnvDependent extends Value {
    def env: Env
    override def envOption = Some(env)
  }

  case class Input(value: String, env: Env) extends EnvDependent {
    def absValue(implicit runtime: RuntimeContext) = value
    def dependencies = Set()
  }

  case class Output(value: String, env: Env, job: Job) extends EnvDependent {
    def absValue(implicit runtime: RuntimeContext) = s"${job.absolutePath}${env.separator}$value"
    def dependencies = Set(job)
  }

  case class Multiple(cases: Cube[Value], env: Env)(implicit runtime: RuntimeContext) extends EnvDependent {
    override def value = cases.map(_.value).allElements.mkString(runtime.IFS_CHAR)
    def absValue(implicit runtime: RuntimeContext) = cases.map(_.absValue).allElements.mkString(runtime.IFS_CHAR)
    override def dependencies = cases.map(_.dependencies).allElements.reduce(_ union _)
  }

}
