package hypermake

import better.files.File
import fansi.Bold
import zio._
import zio.console._
import hypermake.cli.CmdLineAST._
import hypermake.cli.{CLI, CmdLineParser, PlainCLI}
import hypermake.collection.Graph
import hypermake.core.{Job, Plan}
import hypermake.execution.{Executor, RuntimeContext, Status}
import hypermake.semantics.{SemanticParser, Context}
import hypermake.syntax.SyntacticParser
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
      |  list                               : Lists the variables and tasks in this pipeline.
      |  run ${A("$targets")}               : Runs the given tasks or plans (space delimited).
      |  dry-run ${A("$targets")}           : Lists all dependent tasks implicated by the given tasks or plans.
      |  invalidate ${A("$targets")}        : Invalidates the given tasks or plans.
      |  unlock ${A("$targets")}            : Unlocks the given tasks if another instance of Hypermake is unexpectedly killed.
      |  remove ${A("$targets")}            : Removes the output of the given tasks or plans.
      |  mark-as-done ${A("$targets")}      : Mark the given tasks as normally exited.
      |
      |""".stripMargin
  }

  def run(args: List[String]) = {

    val cmd = CmdLineParser.cmdLineParse(args.mkString(" "))

    cmd match {
      case Cmd.Help => putStrLn(helpMessage).orDie as ExitCode(0)
      case Cmd.Version => putStrLn(s"Hypermake $version").orDie as ExitCode(0)

      case Cmd.Run(options, scriptFile, runOptions, subtask, targets) =>

        implicit val runtime: RuntimeContext = RuntimeContext.createFromCLIOptions(options, runOptions)
        val cli = PlainCLI.create()

        implicit val ctx: Context = new Context()
        val parser = new SemanticParser()
        runtime.includePaths foreach { f => parser.semanticParse(runtime.resolveFile(f)) }
        parser.semanticParse(File(scriptFile))

        val jobs = targets flatMap parser.parseTarget flatMap { _.allElements }

        def printJobStatus(job: Job, cli: CLI.Service) = for {
          done <- job.isDone
          _ <- cli.update(job, if (done) Status.Complete else Status.Pending)
        } yield ()

        val eff = for {
          managedCli <- cli
          task <- managedCli.use { cli =>
            val effect = subtask match {
              case Subcommand.List =>
                for {
                  _ <- putStrLn(s"The pipeline in $scriptFile contains:")
                  _ <- putStrLn(s"Variables:")
                  _ <- putStrLn(ctx.allCases.assignments.map { case (name, values) =>
                    s"  $name: ${Bold.On(values.default)} ${values.diff(Set(values.default)).mkString(" ")}"
                  }.mkString("\n"))
                  _ <- putStrLn(s"Tasks:")
                  u <- putStrLn(ctx.tasks.map { case (name, task) =>
                    s"  $name[${task.vars.mkString(", ")}]"
                  }.mkString("\n"))
                } yield u

              case Subcommand.Run =>
                val jobGraph = Graph.traverse[Job](jobs, _.dependentJobs)
                val sortedJobs = jobGraph.topologicalSort
                for {
                  _ <- putStrLn(s"The following ${jobGraph.numNodes} jobs will be run:")
                  _ <- ZIO.foreach_(sortedJobs)(printJobStatus(_, cli))
                  yes <- if (runtime.yes) ZIO.succeed(true) else cli.ask
                  u <- if (yes) Executor.backupJob(sortedJobs) *> Executor.runDAG(jobGraph, cli) else ZIO.succeed(())
                } yield u

              case Subcommand.DryRun =>
                val jobGraph = Graph.traverse[Job](jobs, _.dependentJobs)
                for {
                  _ <- putStrLn(s"The following ${jobGraph.numNodes} jobs are implied in the given target:")
                  u <- ZIO.foreach_(jobGraph.topologicalSort)(printJobStatus(_, cli))
                } yield u

              case Subcommand.Invalidate =>
                val allRuns = File(s"${ctx.localEnv.root}/.runs").children.map(_ / "jobs")
                val allRunJobs = allRuns.flatMap(_.lines).toSet[String].map { s =>
                  parser.parseTask(fastparse.parse(s, SyntacticParser.taskRef1(_)).get.value)
                }
                val jobGraph = Graph.traverse[Job](allRunJobs, _.dependentJobs)
                val jobsToBeInvalidated = Graph.traverse[Job](jobs, jobGraph.outgoingNodes)
                val sortedJobsToBeInvalidated = jobsToBeInvalidated.topologicalSort
                for {
                  _ <- putStrLn(s"The following ${jobsToBeInvalidated.numNodes} jobs will be invalidated:")
                  _ <- ZIO.foreach_(sortedJobsToBeInvalidated)(printJobStatus(_, cli))
                  yes <- if (runtime.yes) ZIO.succeed(true) else cli.ask
                  u <- if (yes) Executor.run(sortedJobsToBeInvalidated)(_.invalidate as true) else ZIO.succeed(())
                } yield u

              case Subcommand.Unlock =>
                for {
                  _ <- putStrLn("The following jobs will be unlocked:")
                  _ <- ZIO.foreach_(jobs)(printJobStatus(_, cli))
                  yes <- if (runtime.yes) ZIO.succeed(true) else cli.ask
                  u <- if (yes) Executor.run(jobs)(_.forceUnlock as true) else ZIO.succeed(())
                } yield u

              case Subcommand.Remove =>
                for {
                  _ <- putStrLn(s"The output of the following jobs will be removed:")
                  _ <- ZIO.foreach_(jobs)(printJobStatus(_, cli))
                  yes <- if (runtime.yes) ZIO.succeed(true) else cli.ask
                  u <- if (yes) Executor.run(jobs) { j =>
                    putStrLn(s"Removing job at ${j.absolutePath}") *> j.removeOutputs as true
                  } else ZIO.succeed(())
                } yield u

              case Subcommand.MarkAsDone =>
                for {
                  _ <- putStrLn("The following jobs will be marked as done:")
                  _ <- ZIO.foreach_(jobs)(printJobStatus(_, cli))
                  yes <- if (runtime.yes) ZIO.succeed(true) else cli.ask
                  u <- if (yes) Executor.run(jobs)(_.markAsDone(cli)) else ZIO.succeed(())
                } yield u

              case Subcommand.ExportShell =>
                putStrLn("Not yet implemented.")

            }
            for {
              _ <- cli.setup
              _ <- effect
              u <- cli.teardown
            } yield u
          }
        } yield task

        (eff as ExitCode(0)).orDie
    }
  }

}
