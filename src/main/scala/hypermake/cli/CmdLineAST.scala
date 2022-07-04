package hypermake.cli

import hypermake.syntax._

object CmdLineAST {

  sealed abstract class Cmd
  object Cmd {
    case object Help    extends Cmd // H
    case object Version extends Cmd // V
    case class Run(
                    options: Seq[Opt],
                    script: String,
                    runOptions: Seq[RunOpt],
                    task: Subcommand,
                    targets: Seq[TaskRefN]
                  ) extends Cmd
  }

  sealed abstract class Opt
  object Opt {
    case class Include(path: String) extends Opt // I
    case class Output(path: String)  extends Opt // O
    case class Shell(shell: String)  extends Opt // S
  }

  sealed abstract class RunOpt
  object RunOpt {
    case class NumJobs(numJobs: Int) extends RunOpt // j
    case object KeepGoing            extends RunOpt // k
    case object Silent               extends RunOpt // s
    case object Verbose              extends RunOpt // v
    case object Yes                  extends RunOpt // y
  }

  sealed trait Subcommand
  object Subcommand {
    case object List extends Subcommand
    case object Run extends Subcommand
    case object DryRun extends Subcommand
    case object Invalidate extends Subcommand
    case object Unlock extends Subcommand
    case object Remove extends Subcommand
    case object MarkAsDone extends Subcommand
    case object ExportShell extends Subcommand
  }

}
