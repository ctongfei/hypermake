package hypermake.semantics

import hypermake.core._
import scala.collection._

/**
 * This is essentially a string potentially paired with multiple dependent tasks.
 */
sealed trait Value {
  def value: String
  def isFile: Boolean
  def envOption: Option[Name]
  def dependencies: Set[Task]
  override def toString = value
}

sealed trait FileValue extends Value {
  def isFile = true
  def env: Name
  override def envOption = Some(env)
}

object Value {

  case class Pure(value: String) extends Value {
    def isFile = false
    def envOption = None
    def dependencies = Set()
  }

  case class TaskInput(value: String, env: Name) extends FileValue {
    def dependencies = Set()
  }

  case class TaskOutput(value: String, env: Name, task: Task) extends FileValue {
    def dependencies = Set(task)
  }

  case class Multiple(cases: Iterable[Value], env: Name)(implicit runtime: RuntimeContext) extends FileValue {
    override def value = cases.map(_.value).mkString(runtime.IFS_CHAR)
    override def dependencies = cases.map(_.dependencies).reduce(_ union _)
  }

}
