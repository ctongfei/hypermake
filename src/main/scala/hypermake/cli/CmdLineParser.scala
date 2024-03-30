package hypermake.cli

import fastparse._

import hypermake.exception._
import hypermake.syntax.Lexical.identifier
import hypermake.syntax._

/**
 * Command line parser of Hypermake.
 */
object CmdLineParser {

  import hypermake.cli.CmdLineAST._
  import hypermake.syntax.Expressions._

  def define[$: P] =
    P {
      ("-D" | "--define") ~ Expressions.identifierPath ~ "=" ~ string
    }
      .map { case (name, value) => Opt.Define(name.str, value) }

  def include[$: P] =
    P {
      ("-I" | "--include") ~ string
    } map Opt.Include

  def shell[$: P] =
    P {
      ("-S" | "--shell") ~ string
    } map Opt.Shell

  def help[$: P]: P[Cmd] = P {
    "--help" | "-H" | "-h"
  } map { _ => Cmd.Help }

  def version[$: P]: P[Cmd] = P {
    "--version" | "-V"
  } map { _ => Cmd.Version }

  def numJobs[$: P] =
    P {
      ("-j" | "--jobs") ~ Lexical.digit.rep.!
    } map { j => RunOpt.NumJobs(j.toInt) }

  def switch[$: P](short: String, long: String, out: RunOpt): P[RunOpt] =
    P {
      long | short
    } map { _ => out }

  def opt[$: P]: P[Opt] = P {
    define | include | shell
  }

  def runtimeOpts[$: P] = P {
    numJobs |
      switch("-k", "--keep-going", RunOpt.KeepGoing) |
      switch("-s", "--silent", RunOpt.Silent) |
      switch("-v", "--verbose", RunOpt.Verbose) |
      switch("-y", "--yes", RunOpt.Yes)
  }

  def target[$: P] = Expressions.taskRef

  def fileNameString[$: P] = P {
    Lexical.quotedString | Lexical.pathString
  }

  def command[$: P]: P[Subcommand] = P {
    "list".! | "get-path".! | "run".! | "dry-run".! | "invalidate".! | "unlock".! | "remove".! | "mark-as-done".! // "export-shell".!
  } map {
    case "list"         => Subcommand.List
    case "get-path"     => Subcommand.GetPath
    case "run"          => Subcommand.Run
    case "dry-run"      => Subcommand.DryRun
    case "invalidate"   => Subcommand.Invalidate
    case "unlock"       => Subcommand.Unlock
    case "remove"       => Subcommand.Remove
    case "mark-as-done" => Subcommand.MarkAsDone
    // case "export-shell" => Subcommand.ExportShell
  }

  def run[$: P] = P {
    opt.rep ~ fileNameString ~ command ~ runtimeOpts.rep ~ target.rep ~ runtimeOpts.rep
  }.map { case (opt, scriptFile, subcommand, runOpts1, targets, runOpts2) =>
    Cmd.Run(opt, scriptFile, runOpts1 ++ runOpts2, subcommand, targets)
  }

  def cmdArgs[$: P] = P {
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
