package hypermake.cli

import fastparse._
import hypermake.exception._
import hypermake.syntax._

/**
 * Command line parser of Forge.
 *
 * Command line API:
 *  - `forge [optional script name] [options] mode [tasks]`
 *
 *
 * Forge has the following command line options:
 *  - `-S $SHELL`: Default shell to run (default: bash)
 *  - `-I $INCLUDE_PATHS`: Paths to find Forge imports (As in `gcc` / `make`)
 *  - `-j $NUMBER_OF_PARALLEL_TASKS`: Number of tasks to run simultaneously
 *  - `-v`: Verbose
 *  - `-y`: Yes: run jobs specified without prompt
 */
object CmdLineParser {

  import fastparse.ScriptWhitespace._
  import hypermake.cli.CmdLineAST._
  import hypermake.syntax.SyntacticParser._

  def include[_: P] =
    P { ("-I" | "--include") ~ string } map { i => Opt.Include(i) }

  def shell[_: P] =
    P { ("-S" | "--shell") ~ string } map { s => Opt.Shell(s) }

  def help[_: P]: P[Cmd] = P { "--help" | "-H" | "-h" } map { _ => Cmd.Help }
  def version[_: P]: P[Cmd] = P { "--version" | "-V" } map { _ => Cmd.Version }

  def numJobs[_: P] =
    P { "-j" ~ Lexer.digit.rep.! } map { j => RunOpt.NumJobs(j.toInt) }

  def keepGoing[_: P]: P[RunOpt] = P { "--keep-going" | "-k" } map { _ => RunOpt.KeepGoing }

  def dryRun[_: P]: P[RunOpt] = P { "--dry-run" | "-n" } map { _ => RunOpt.DryRun }

  def silent[_: P]: P[RunOpt] = P { "--silent" | "-s" } map { _ => RunOpt.Silent }

  def verbose[_: P]: P[RunOpt] = P { "--verbose" | "-v" } map { _ => RunOpt.Verbose }

  def yes[_: P]: P[RunOpt] = P { "--yes" | "-y" } map { _ => RunOpt.Yes }


  def opt[_: P]: P[Opt] = P { include | shell }
  def runtimeOpts[_: P] = P { numJobs | keepGoing | dryRun | silent | verbose | yes }

  def target[_: P] = SyntacticParser.taskRefN

  def fileNameString[_: P] = P { Lexer.quotedString | Lexer.pathString }

  def command[_: P] = P { "run".! | "invalidate".! | "mark-as-done".! | "export-shell".! }

  def run[_: P] = P {
    opt.rep ~ fileNameString ~ command ~ runtimeOpts.rep ~ target.rep ~ runtimeOpts.rep
  }.map { case (opt, scriptFile, cmd, runOpts1, targets, runOpts2) =>
    val subtask = cmd match {
      case "run"          => Subtask.Run(targets)
      case "invalidate"   => Subtask.Invalidate(targets)
      case "mark-as-done" => Subtask.MarkAsDone(targets)
      case "export-shell" => Subtask.ExportShell(targets)
    }
    Cmd.Run(opt, scriptFile, runOpts1 ++ runOpts2, subtask)
  }

  def cmdArgs[_: P] = P {
    (version | help | run) ~ End
  }

  def cmdLineParse(args: String): Cmd = {
    parse(args, cmdArgs(_)) match {
      case Parsed.Success(a, _) => a
      case f: Parsed.Failure => throw ParsingException(f)
    }
  }

}
