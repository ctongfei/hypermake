package hypermake.cli

import fastparse._
import hypermake.exception._
import hypermake.syntax.Lexer.identifier
import hypermake.syntax._

/**
 * Command line parser of Hypermake.
 */
object CmdLineParser {

  import fastparse.ScriptWhitespace._
  import hypermake.cli.CmdLineAST._
  import hypermake.syntax.SyntacticParser._

  def define[_: P] =
    P { ("-D" | "--define") ~ Lexer.identifier ~ "=" ~ string }
      .map { case (name, value) => Opt.Define(name.name, value) }

  def include[_: P] =
    P { ("-I" | "--include") ~ string } map Opt.Include

  def shell[_: P] =
    P { ("-S" | "--shell") ~ string } map Opt.Shell

  def help[_: P]: P[Cmd] = P { "--help" | "-H" | "-h" } map { _ => Cmd.Help }
  def version[_: P]: P[Cmd] = P { "--version" | "-V" } map { _ => Cmd.Version }

  def numJobs[_: P] =
    P { "-j" ~ Lexer.digit.rep.! } map { j => RunOpt.NumJobs(j.toInt) }

  def switch[_: P](short: String, long: String, out: RunOpt): P[RunOpt] =
    P { long | short } map { _ => out }

  def opt[_: P]: P[Opt] = P { define | include | shell }

  def runtimeOpts[_: P] = P {
    numJobs |
      switch("-k", "--keep-going", RunOpt.KeepGoing) |
      switch("-s", "--silent", RunOpt.Silent) |
      switch("-v", "--verbose", RunOpt.Verbose) |
      switch("-y", "--yes", RunOpt.Yes)
  }

  def target[_: P] = SyntacticParser.taskRefN

  def fileNameString[_: P] = P { Lexer.quotedString | Lexer.pathString }

  def command[_: P]: P[Subcommand] = P {
    "list".! | "run".! | "dry-run".! | "invalidate".! | "unlock".! | "remove".! | "mark-as-done".! // "export-shell".!
  } map {
    case "list"         => Subcommand.List
    case "run"          => Subcommand.Run
    case "dry-run"      => Subcommand.DryRun
    case "invalidate"   => Subcommand.Invalidate
    case "unlock"       => Subcommand.Unlock
    case "remove"       => Subcommand.Remove
    case "mark-as-done" => Subcommand.MarkAsDone
    case "export-shell" => Subcommand.ExportShell
  }

  def run[_: P] = P {
    opt.rep ~ fileNameString ~ command ~ runtimeOpts.rep ~ target.rep ~ runtimeOpts.rep
  }.map { case (opt, scriptFile, subcommand, runOpts1, targets, runOpts2) =>
    Cmd.Run(opt, scriptFile, runOpts1 ++ runOpts2, subcommand, targets)
  }

  def cmdArgs[_: P] = P {
    (version | help | run) ~ End
  }

  def cmdLineParse(args: String): Cmd = {
    parse(args, cmdArgs(_)) match {
      case Parsed.Success(a, _) => a
      case f: Parsed.Failure =>
        println(hypermake.Main.helpMessage)
        throw ParsingException(f)
    }
  }

}
