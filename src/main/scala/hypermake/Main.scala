package hypermake

import better.files.File
import fansi.Bold
import zio._
import zio.console._
import hypermake.cli.CmdLineAST.{Cmd, Opt, RunOpt, Subcommand}
import hypermake.cli.{CLI, CmdLineParser, PlainCLI}
import hypermake.collection.Graph
import hypermake.core.{Job, Plan}
import hypermake.execution.{Executor, RuntimeContext, Status}
import hypermake.semantics.{SemanticParser, SymbolTable}
import hypermake.util.printing._

object Main extends App {

  val version = "0.1.0"

  lazy val helpMessage = {

    s"""
      | Hypermake $version
      | Usage:
      |   ${Bold.On("hypermake")} [${O("options")}] [Hypermake script] <${C("command")}> [${RO("running options")}] [targets]
      |
      | Options:
      |  -I ${A("$file")}, --include=${A("$file")}  : Includes the specific Hypermake script ${A("file")} to parse.
      |  -H, --help                 : Prints this message and exit.
      |  -S, --shell                : Specify outer shell to use. By default this is "bash".
      |  -V, --version              : Shows Hypermake version and exit.
      |
      | Running options:
      |  -j ${A("$n")}, --jobs=${A("$n")}           : Allow ${A("n")} jobs running in parallel at once.
      |  -k, --keep-going           : Keep going even when some jobs failed.
      |  -s, --silent               : Silent mode: redirect stdout and stderr to files without printing them.
      |  -v, --verbose              : Verbose mode.
      |  -y, --yes                  : Automatic "yes" to prompts.
      |
      | Commands:
      |  run ${A("$targets")}               : Runs the given tasks or plans (space delimited).
      |  dry-run ${A("$targets")}           : Lists all dependent tasks implicated by the given tasks or plans.
      |  invalidate ${A("$targets")}        : Invalidates the given tasks or plans.
      |  remove ${A("$targets")}            : Removes the output of the given tasks or plans.
      |  mark-as-done ${A("$targets")}      : Mark the given tasks as normally exited.
      |  export-shell ${A("$targets")}      : Generates an equivalent shell script that runs the given tasks.
      |
      |""".stripMargin
  }


  def run(args: List[String]) = {

    val cmd = CmdLineParser.cmdLineParse(args.mkString(" "))

    cmd match {
      case Cmd.Help => putStrLn(helpMessage) as ExitCode(0)
      case Cmd.Version => putStrLn(s"HyperMake $version") as ExitCode(0)
      case Cmd.Run(options, scriptFile, runOptions, subtask) =>

        implicit val runtime: RuntimeContext = RuntimeContext.create(
          includePaths = options.collect { case Opt.Include(f) => f },
          shell = options.collectFirst { case Opt.Shell(s) => s }.getOrElse("bash"),
          numParallelJobs = runOptions.collectFirst { case RunOpt.NumJobs(j) => j }.getOrElse(1),
          keepGoing = runOptions contains RunOpt.KeepGoing,
          silent = runOptions contains RunOpt.Silent,
          yes = runOptions contains RunOpt.Yes
        )
        val cli = PlainCLI.create()

        implicit val ctx: SymbolTable = new SymbolTable()
        val parser = new SemanticParser()
        runtime.includePaths foreach { f => parser.semanticParse(File(f)) }
        parser.semanticParse(File(scriptFile))

        def printJobStatus(job: Job, cli: CLI.Service) = for {
          done <- job.isDone
          _ <- cli.update(job, if (done) Status.Complete else Status.Pending)
        } yield ()

        val eff = for {
          managedCli <- cli
          task <- managedCli.use { cli =>
            subtask match {
              case Subcommand.Run(ts) =>
                val jobGraph = new Plan(ts flatMap parser.parseTarget).dependencyGraph
                for {
                  _ <- cli.initialize
                  _ <- putStrLn("The following jobs will be run:")
                  _ <- ZIO.foreach_(jobGraph.topologicalSort)(printJobStatus(_, cli))
                  yes <- if (runtime.yes) ZIO.succeed(true) else cli.ask("Continue? [y/n]: ")
                  u <- if (yes) Executor.runDAG(jobGraph, cli) else ZIO.succeed(())
                  _ <- cli.tearDown
                } yield u

              case Subcommand.DryRun(ts) =>
                val jobGraph = new Plan(ts flatMap parser.parseTarget).dependencyGraph
                for {
                  _ <- cli.initialize
                  _ <- putStrLn("The following jobs are implied in the given target:")
                  u <- ZIO.foreach_(jobGraph.topologicalSort)(printJobStatus(_, cli))
                  _ <- getStrLn
                  _ <- cli.tearDown
                } yield u

              case Subcommand.Invalidate(ts) => ???

              case Subcommand.MarkAsDone(ts) =>
                val jobs = ts flatMap parser.parseTarget flatMap { _.allElements }
                for {
                  _ <- cli.initialize
                  _ <- putStrLn("The following jobs are to be marked as done:")
                  _ <- ZIO.foreach_(jobs)(printJobStatus(_, cli))
                  yes <- if (runtime.yes) ZIO.succeed(true) else cli.ask("Continue? [y/n]: ")
                  u <- if (yes) Executor.run(jobs)(_.markAsDone(cli)) else ZIO.succeed(())
                  _ <- cli.tearDown
                } yield u

              case Subcommand.ExportShell(ts) => ???

            }
          }
        } yield task


        eff as ExitCode(0) orElseSucceed ExitCode(1)
    }
  }

}
