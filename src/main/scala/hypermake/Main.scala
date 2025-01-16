package hypermake

import better.files.File
import zio._
import zio.console._

import hypermake.cli.CmdLineAST._
import hypermake.cli._
import hypermake.collection._
import hypermake.core._
import hypermake.execution._
import hypermake.semantics._
import hypermake.syntax._
import hypermake.util.StdSinks
import hypermake.util.printing._

/** Entrypoint of Hypermake. */
object Main extends App {

  val version = "0.1.0"

  lazy val headerMessage = s"${B("HyperMake")} $version -- A parameterized pipeline manager"

  lazy val helpMessage = {
    s"""
       |$headerMessage
       | ${B("Usage:")}
       |   ${CC("hypermake")} [${O("options")}] [Hypermake script] <${C("command")}> [${RO("running options")}] [targets]
       |
       | ${B("Options:")}
       |  -D ${K("$k")}=${V("$v")}, --define ${K("$k")}=${V("$v")}   : Defines an additional variable ${K("k")} = ${V("v")} in the script.
       |  -I ${V("$file")}, --include ${V("$file")}  : Includes the specific directories ${V("file")} when resolving imports.
       |  -S ${V("$path")}, --shell ${V("$path")}    : Specifies default shell to use. By default this is "${V("bash -e")}".
       |  -H, --help                 : Prints this message and exit.
       |  -V, --version              : Shows HyperMake version and exit.
       |
       | ${B("Commands:")}
       |  list                       : Lists the variables and tasks in this pipeline.
       |  get-path ${V("$targets")}          : Prints the path of the given tasks.
       |  run ${V("$targets")}               : Runs the given tasks or plans (space delimited).
       |  dry-run ${V("$targets")}           : Lists all dependent tasks implicated by the given tasks or plans.
       |  invalidate ${V("$targets")}        : Invalidates the given tasks or plans.
       |  unlock ${V("$targets")}            : Unlocks the given tasks if another instance of HyperMake is unexpectedly killed.
       |  remove ${V("$targets")}            : Removes the output of the given tasks or plans.
       |  mark-as-done ${V("$targets")}      : Marks the given tasks as if they have exited normally.
       |
       | ${B("Running options:")}
       |  -j ${V("$n")}, --jobs ${V("$n")}           : Allow ${V("n")} jobs running in parallel at once.
       |  -k, --keep-going           : Keep going even when some jobs failed.
       |  -s, --silent               : Silent mode: redirect stdout and stderr to files.
       |  -v, --verbose              : Verbose mode.
       |  -y, --yes                  : Automatic "yes" to prompts.
       |""".stripMargin
    // TODO: timestamp (-t) and checksum (-c)
  }

  def run(args: List[String]) = {

    val cmd = CmdLineParser.cmdLineParse(args.mkString(" "))

    cmd match {
      case Cmd.Help    => putStrLn(helpMessage).orDie as ExitCode(0)
      case Cmd.Version => putStrLn(s"HyperMake $version").orDie as ExitCode(0)

      case Cmd.Run(options, scriptFile, runOptions, subtask, targets) =>
        implicit val runtime: RuntimeConfig =
          RuntimeConfig.createFromCLIOptions(options, runOptions)
        val cli = PlainCLI.create(style = Style.Plain)

        // Constructs a semantic parser and its accompanying parsing context
        implicit val ctx: Context = new Context()
        val parser = new SemanticParser(ctx.root)
        // TODO: Defines variables specified with the -D switch
        // parser.semanticParse(parser.readLinesToStmts(runtime.definedVars.map { case (k, v) => s"$k = $v" }))

        parser.semanticParseFile(File(scriptFile), scope = ctx.root)
        parser.addFallbackLocalFsDefs()

        val jobs = targets flatMap parser.parseTarget flatMap { _.allElements }

        def showTaskCube(pct: PointedTaskTensor) = {
          val name = if (pct.name contains "@") BU(pct.name) else B(pct.name)
          "• " + name + (if (pct.vars.isEmpty) ""
                         else pct.vars.map(n => Kx(n.name)).mkString("[", ", ", "]"))
        }

        val eff = for {
          managedCli <- cli
          task <- managedCli.use { cli =>
            implicit val std: StdSinks = cli.globalSinks
            val effect = subtask match {
              case Subcommand.List =>
                for {
                  _ <- putStrLn(headerMessage)
                  _ <- putStrLn(B("Pipeline file: ") + O(scriptFile))
                  _ <- putStrLn(B("\nVariables:"))
                  _ <- putStrLn(
                    ctx.allCases.assignments
                      .map { case (name, values) =>
                        val valuesStr = if (values.size == 1)
                          V(values.default)
                        else V(values.default) + " " + values.diff(Set(values.default)).map(Vx).mkString(" ")
                        s"  • ${K(name.name)}: { $valuesStr }"
                      }
                      .mkString("\n")
                  )
                  _ <- putStrLn(B("\nTasks:"))
                  s <- {
                    val g = Graph.explore[PointedTaskTensor](
                      ctx.root.tasks.values,
                      _.dependentTaskTensors(ctx)
                    )
                    g.toStringIfAcyclic(t => ZIO.succeed(showTaskCube(t)), indent = 2)
                  }
                  u <- putStrLn(s)
                } yield u

              case Subcommand.GetPath =>
                for {
                  u <- ZIO.foreach_(jobs.map(_.absolutePath))(putStrLn(_))
                } yield u

              case Subcommand.Run =>
                val jobGraph = Graph.exploreBidirectionally[Job](
                  jobs,
                  prev = j => j.dependentJobs ++ j.services.map(_.start),
                  next = j => j.services.map(_.stop)
                )
                val sortedJobs = jobGraph.topologicalSort.toIndexedSeq
                val ephemeralJobs = sortedJobs.filter(_.ephemeral)

                for {
                  _ <- putStrLn(headerMessage)
                  _ <- putStrLn(
                    s"The following ${jobGraph.numNodes} jobs are implied in the given target:"
                  )
                  s <- jobGraph.toStringIfAcyclic(_.statusString(cli))
                  _ <- putStrLn(s)
                  yes <- if (runtime.yes) ZIO.succeed(true) else cli.ask
                  u <- {
                    if (yes)
                      Executor.recordJobsRun(sortedJobs, cli) *> Executor.runDAG(jobGraph, cli)
                    else ZIO.succeed(())
                  }
                  _ <- ZIO.foreach_(ephemeralJobs)(_.removeOutputs)
                } yield u

              case Subcommand.DryRun =>
                val jobGraph = Graph.exploreBidirectionally[Job](
                  jobs,
                  prev = j => j.dependentJobs ++ j.services.map(_.start),
                  next = j => j.services.map(_.stop)
                )
                for {
                  _ <- putStrLn(headerMessage)
                  _ <- putStrLn(
                    s"The following ${jobGraph.numNodes} jobs are implied in the given target:"
                  )
                  s <- jobGraph.toStringIfAcyclic(_.statusString(cli))
                  u <- putStrLn(s)
                } yield u

              case Subcommand.Invalidate =>
                val allRuns = File(s"${ctx.local.root}/.runs").children.map(_ / "jobs").toSeq
                val allRunJobs = allRuns.flatMap(_.lines).toSet[String].flatMap { s =>
                  parser.parseTask(fastparse.parse(s, Expressions.taskRef(_)).get.value)
                }
                val jobGraph = Graph.explore[Job](allRunJobs, _.dependentJobs)
                val jobsToBeInvalidated = Graph.explore[Job](jobs, jobGraph.outgoingNodes)
                val sortedJobsToBeInvalidated = jobsToBeInvalidated.topologicalSort
                for {
                  _ <- putStrLn(headerMessage)
                  _ <- putStrLn(
                    s"The following ${jobsToBeInvalidated.numNodes} jobs will be invalidated:"
                  )
                  _ <- ZIO.foreach_(sortedJobsToBeInvalidated)(_.printStatus(cli))
                  yes <- if (runtime.yes) ZIO.succeed(true) else cli.ask
                  u <-
                    if (yes) Executor.run(sortedJobsToBeInvalidated)(_.invalidate as true)
                    else ZIO.succeed(())
                } yield u

              case Subcommand.Unlock =>
                for {
                  _ <- putStrLn(headerMessage)
                  _ <- putStrLn("The following jobs will be unlocked:")
                  _ <- ZIO.foreach_(jobs)(_.printStatus(cli))
                  yes <- if (runtime.yes) ZIO.succeed(true) else cli.ask
                  u <- if (yes) Executor.run(jobs)(_.forceUnlock as true) else ZIO.succeed(())
                } yield u

              case Subcommand.Remove =>
                for {
                  _ <- putStrLn(headerMessage)
                  _ <- putStrLn(s"The output of the following jobs will be removed:")
                  _ <- ZIO.foreach_(jobs)(_.printStatus(cli))
                  yes <- if (runtime.yes) ZIO.succeed(true) else cli.ask
                  u <-
                    if (yes) Executor.run(jobs) { j =>
                      putStrLn(s"Removing job at ${j.absolutePath}") *> j.removeOutputs as true
                    }
                    else ZIO.succeed(())
                } yield u

              case Subcommand.MarkAsDone =>
                for {
                  _ <- putStrLn(headerMessage)
                  _ <- putStrLn("The following jobs will be marked as done:")
                  _ <- ZIO.foreach_(jobs)(_.printStatus(cli))
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

        eff.exitCode
    }
  }

}
