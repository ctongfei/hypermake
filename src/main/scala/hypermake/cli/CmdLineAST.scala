package hypermake.cli

import hypermake.collection.Axis
import hypermake.syntax.ast._

object CmdLineAST {

  sealed abstract class Cmd

  object Cmd {
    case object Help extends Cmd // H
    case object Version extends Cmd // V
    case class RunTarget(
        subcommand: SubcommandType,
        options: Seq[Opt],
        runOptions: Seq[RunOpt],
        targets: Seq[TaskRef]
    ) extends Cmd
    case class DescribeTarget(
        options: Seq[Opt],
        describeOptions: Seq[DescribeOpt],
        targets: Seq[TaskRef]
    ) extends Cmd
  }

  sealed abstract class Opt

  object Opt {
    case class Define(name: String, value: String) extends Opt // -D name=value
    case class File(path: String) extends Opt // F
    case class IncludeDir(path: String) extends Opt // I
    case class IncludeGit(repo: String) extends Opt // G
    case class Shell(shell: String) extends Opt // S
  }

  /** Options that impacts the definition of the pipeline. */
  sealed abstract class RunOpt

  object RunOpt {
    case class NumJobs(numJobs: Int) extends RunOpt // j
    case object KeepGoing extends RunOpt // k
    case object Silent extends RunOpt // s
    case object Verbose extends RunOpt // v
    case object Yes extends RunOpt // y
  }

  sealed abstract class DescribeOpt

  object DescribeOpt {
    case class OutputFormat(format: String) extends DescribeOpt // o
  }

  sealed trait SubcommandType

  sealed trait RunSubcommandType extends SubcommandType

  object SubcommandType {
    case object List extends SubcommandType
    case object GetPath extends SubcommandType
    case object Describe extends SubcommandType

    case object Run extends RunSubcommandType
    case object DryRun extends RunSubcommandType
    case object Invalidate extends RunSubcommandType
    case object Unlock extends RunSubcommandType
    case object Remove extends RunSubcommandType
    case object Touch extends RunSubcommandType
  }

}
