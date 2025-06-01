package hypermake.cli

import fastparse._

import hypermake.exception._
import hypermake.syntax.Lexical.identifier
import hypermake.syntax._

/** Command line parser of Hypermake. */
object CmdLineParser {

  import hypermake.cli.CmdLineAST._
  import hypermake.syntax.Expressions._

  def define[$: P] = P {
    ("-D" | "--define") ~ Expressions.identifierPath ~ "=" ~ string
  } map { case (name, value) => Opt.Define(name.str, value) }

  def file[$: P] = P {
    ("-F" | "--file") ~ fileNameString
  } map Opt.File

  def include[$: P] =
    P {
      ("-I" | "--include-dir") ~ fileNameString
    } map Opt.IncludeDir

  def includeGit[$: P] =
    P {
      ("-G" | "--include-git") ~ (Lexical.noSpaceString | Lexical.quotedString)
    } map Opt.IncludeGit

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

  def outputFormat[$: P] =
    P {
      ("-o" | "--output") ~ ("json".! | "sh".!)
    } map DescribeOpt.OutputFormat

  def switch[$: P](short: String, long: String, out: RunOpt): P[RunOpt] =
    P {
      long | short
    } map { _ => out }

  def opt[$: P]: P[Opt] = P {
    file | define | include | includeGit | shell
  }

  def runtimeOpts[$: P] = P {
    numJobs |
      switch("-k", "--keep-going", RunOpt.KeepGoing) |
      switch("-s", "--silent", RunOpt.Silent) |
      switch("-v", "--verbose", RunOpt.Verbose) |
      switch("-y", "--yes", RunOpt.Yes)
  }

  def describeOpts[$: P] = P {
    outputFormat
  }

  def target[$: P] = Expressions.taskRef

  def fileNameString[$: P] = P {
    Lexical.quotedString | Lexical.pathString
  }

  def runSubcommand[$: P]: P[RunSubcommandType] = P {
    "run".! | "dry-run".! | "invalidate".! | "unlock".! | "remove".! | "touch".!
  } map {
    case "run"        => SubcommandType.Run
    case "dry-run"    => SubcommandType.DryRun
    case "invalidate" => SubcommandType.Invalidate
    case "unlock"     => SubcommandType.Unlock
    case "remove"     => SubcommandType.Remove
    case "touch"      => SubcommandType.Touch
  }

  def runTarget[$: P] = P {
    opt.rep ~ runSubcommand ~ runtimeOpts.rep ~ target.rep ~ runtimeOpts.rep
  }.map { case (opt, subcommand, runOpts1, targets, runOpts2) =>
    Cmd.RunTarget(subcommand, opt, runOpts1 ++ runOpts2, targets)
  }

  def describe[$: P] = P {
    opt.rep ~ "describe" ~ describeOpts.rep ~ target.rep ~ describeOpts.rep
  }.map { case (opt, descOpts1, targets, descOpts2) =>
    Cmd.DescribeTarget(opt, descOpts1 ++ descOpts2, targets)
  }

  def cmdArgs[$: P] = P {
    (version | help | runTarget) ~ End
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
