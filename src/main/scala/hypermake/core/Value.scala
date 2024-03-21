package hypermake.core

import scala.collection._

import hypermake.collection._
import hypermake.exception.ValueNotPureException
import hypermake.execution._

/** This is essentially a string potentially paired with multiple dependent tasks.
  */
sealed trait Value {
  def value: String

  def absValue(implicit runtime: RuntimeConfig): String

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
    def absValue(implicit runtime: RuntimeConfig) = value

    def optFs = None

    def dependencies = Set()
  }

  case class PackageOutput(pack: Package) extends FileSysAgnostic {
    def value = pack.outputFileName._2.value

    def absValue(implicit runtime: RuntimeConfig) = ???

    def optFs = None

    def dependencies = Set()

    def on(fs: FileSys): Output = Output(value, fs, pack.on(fs))
  }

  sealed trait FileSysDependent extends Value {
    def fileSys: FileSys

    override def optFs = Some(fileSys)
  }

  case class Input(value: String, fileSys: FileSys) extends FileSysDependent {
    def absValue(implicit runtime: RuntimeConfig) = value

    def dependencies = Set()
  }

  case class Output(value: String, fileSys: FileSys, job: Job) extends FileSysDependent {
    def absValue(implicit runtime: RuntimeConfig) = s"${job.absolutePath}${fileSys.separator}$value"

    def dependencies = Set(job)
  }

  case class Multiple(cases: Tensor[Value], fileSys: FileSys)(implicit runtime: RuntimeConfig)
      extends FileSysDependent {
    override def value = cases.map(_.value).allElements.mkString(runtime.IFS_CHAR)

    def absValue(implicit runtime: RuntimeConfig) =
      cases.map(_.absValue).allElements.mkString(runtime.IFS_CHAR)

    override def dependencies = cases.map(_.dependencies).allElements.reduce(_ union _)
  }

}
