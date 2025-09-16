package hypermake.core

import scala.collection._

import hypermake.collection._
import hypermake.exception.ValueNotPureException
import hypermake.execution._

/**
 *  A string value in HyperMake.
 * Essentially a string potentially paired with its dependent tasks.
 */
sealed trait Value {

  /** The string value. If it is a path, it is relative to the root of the file system. */
  def value: String

  def absValue: String

  def isPath: Boolean = optFs.isDefined

  def optFs: Option[FileSys]

  def dependencies: Set[Job]

  override def toString = value

  def asIfPure: Value.Pure = this match {
    case x: Value.Pure => x
    case _             => throw ValueNotPureException(value)
  }
}

object Value {

  sealed trait FileSysAgnostic extends Value

  case class Pure(value: String) extends FileSysAgnostic {
    def absValue = value

    def optFs = None

    def dependencies = Set()
  }

  sealed trait FileSysDependent extends Value {
    def fileSys: FileSys

    override def optFs = Some(fileSys)
  }

  case class Input(value: String, fileSys: FileSys) extends FileSysDependent {
    def absValue = value

    def dependencies = Set()
  }

  case class Output(value: String, fileSys: FileSys, job: Job, service: Option[Service] = None) extends FileSysDependent {
    def absValue =
      s"${fileSys.root}${fileSys./}${job.path}${fileSys./}$value"

    def dependencies = Set(job)
  }

  case class Multiple(cases: Tensor[Value], fileSys: FileSys) extends FileSysDependent {
    override def value = cases.map(_.value).allElements.mkString(" ")

    def absValue =
      cases.map(_.absValue).allElements.mkString(" ")

    override def dependencies = cases.map(_.dependencies).allElements.reduce(_ union _)
  }

}
