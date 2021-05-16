package hypermake.cli

import better.files._
import hypermake.cli.CmdLineAST._
import hypermake.execution._
import hypermake.core._
import hypermake.semantics.{ParsingContext, SemanticParser}
import hypermake.syntax.TaskRefN
import hypermake.util.printing._

object Main extends App {

  val version = "0.1.0"

  lazy val helpMessage = {

    s"""
      | Hypermake $version
      | Usage: hypermake [${O("options")}] [Hypermake script] <${C("command")}> [${RO("running options")}] [targets]
      |
      | Options:
      |  -I <file>, --include=<file>   : Includes the specific Hypermake script to parse.
      |  -H, --help                    : Prints this message and exit.
      |  -S, --shell                   : Specify outer shell to use. By default this is "bash".
      |  -V, --version                 : Shows Forge version and exit.
      |
      | Running options:
      |  -j <n>, --jobs=<n>            : Allow n jobs running in parallel at once.
      |  -k, --keep-going              : Keep going even when some jobs failed.
      |  -n, --dry-run                 : Does not actually run any recipe; just prints them.
      |  -s, --silent                  : Does not print recipes.
      |  -v, --verbose                 : Verbose mode.
      |  -y, --yes                     : Automatic "yes" to prompts.
      |
      | Commands:
      |  print <var>                   : Prints the specific Hypermake script variable.
      |  run <tasks or plans>          : Runs the given tasks or plans (space delimited).
      |  invalidate <tasks or plans>   : Invalidates the given tasks or plans.
      |  remove <tasks>                : Removes the output of the given tasks or plans.
      |  mark-as-done <tasks or plans> : Mark the given tasks as normally exited.
      |  export-shell <tasks or plans> : Generates an equivalent shell script that runs the given tasks.
      |
      |""".stripMargin
  }

  val cmd = CmdLineParser.cmdLineParse(args.mkString(" "))

  cmd match {
    case Cmd.Help    => println(helpMessage)
    case Cmd.Version => println(s"HyperMake $version")
    case Cmd.Run(options, scriptFile, runOptions, subtask) =>

      implicit val runtime: RuntimeContext = RuntimeContext.create(
        includePaths = options.collect { case Opt.Include(f) => f },
        shell = options.collectFirst { case Opt.Shell(s) => s }.getOrElse("bash"),
        numParallelJobs = runOptions.collectFirst { case RunOpt.NumJobs(j) => j }.getOrElse(1),
        keepGoing = runOptions contains RunOpt.KeepGoing,
        dryRun = runOptions contains RunOpt.DryRun,
        silent = runOptions contains RunOpt.Silent,
        yes = runOptions contains RunOpt.Yes
      )

      runtime._println(runtime.toString)
      runtime._println("Parsing Hypermake scripts...")
      implicit val ctx: ParsingContext = new ParsingContext()
      val parser = new SemanticParser()
      import parser._
      runtime.includePaths foreach { f => parser.semanticParse(File(f)) }
      parser.semanticParse(File(scriptFile))
      runtime._println("Parsing complete.")

      val task = subtask match {

        case Subtask.Run(ts) =>
          val jobGraph = new Plan(ts flatMap parseTarget).dependencyGraph
          Executor.runDAG(jobGraph)

        case Subtask.Invalidate(ts) => ???

        case Subtask.MarkAsDone(ts) => ???

        case Subtask.ExportShell(ts) => ???

      }
      zio.Runtime.global.unsafeRun(task)

  }

}
