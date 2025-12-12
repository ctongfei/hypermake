package hypermake

import better.files.File
import upickle.default._
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
       |   ${CC("hypermake")} [${O("options")}] [HyperMake script] <${C("command")}> [${RO("running options")}] [targets]
       |
       | ${B("Options:")}
       |  -D ${K("$k")}=${V("$v")}, --define ${K("$k")}=${V("$v")}   : Defines an additional variable ${K("k")} = ${V("v")} in the script.
       |  -F ${V("$file")}, --file ${V("$file")}    : Specifies the HyperMake script file. If omitted, HyperMake will look for a unique *.hm file in the current directory.
       |  -I ${V("$file")}, --include ${V("$file")}  : Includes the specific directories ${V("file")} when resolving imports.
       |  -S ${V("$path")}, --shell ${V("$path")}    : Specifies default shell to use. By default this is "${V("bash -e")}".
       |  -H, --help                 : Prints this message and exit.
       |  -V, --version              : Shows HyperMake version and exit.
       |
       | ${B("Commands:")}
       |  list                       : Lists the variables and tasks in this pipeline.
       |  describe ${V("$job")}              : Describes the specific job.
       |  get-path ${V("$targets")}          : Prints the path of the given tasks.
       |  run ${V("$targets")}               : Runs the given tasks or plans (space delimited).
       |  dry-run ${V("$targets")}           : Lists all dependent tasks implicated by the given tasks or plans.
       |  invalidate ${V("$targets")}        : Invalidates the given tasks or plans.
       |  unlock ${V("$targets")}            : Unlocks the given tasks if another instance of HyperMake is unexpectedly killed.
       |  remove ${V("$targets")}            : Removes the output of the given tasks or plans.
       |  touch ${V("$targets")}             : Marks the given tasks as if they have exited normally.
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

      case Cmd.RunTarget(subtask, options, runOptions, targets) =>
        implicit val runtime: RuntimeConfig =
          RuntimeConfig.createFromCliOptions(options, runOptions)
        val cli = PlainCLI.create(style = Style.Plain)

        // Constructs a semantic parser and its accompanying parsing context
        implicit val ctx: Context = new Context()
        val parser = new SemanticParser(ctx.root)
        // TODO: Defines variables specified with the -D switch
        // parser.semanticParse(parser.readLinesToStmts(runtime.definedVars.map { case (k, v) => s"$k = $v" }))

        parser.semanticParseFile(File(runtime.pipelineFile), scope = ctx.root)
        parser.addFallbackLocalFsDefs()

        val targetJobs = targets flatMap parser.parseTarget flatMap { _.allElements }

        def showTaskCube(pct: PointedTaskTensor) = {
          val name = if (pct.name contains "@") BU(pct.name) else B(pct.name)
          "• " + name + (if (pct.vars.isEmpty) ""
                         else pct.vars.map(n => Kx(n.name)).mkString("[", ", ", "]"))
        }

        val eff = for {
          managedCli <- cli
          task <- managedCli.use { implicit cli =>
            implicit val std: StdSinks = cli.globalSinks
            val effect = subtask match {
              case SubcommandType.List =>
                for {
                  _ <- putStrLn(headerMessage)
                  _ <- putStrLn(B("Pipeline file: ") + O(runtime.pipelineFile))
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
                      _.dependentTaskTensors(ctx) // TODO: better way of doing this
                    )
                    g.toStringIfAcyclic(t => ZIO.succeed(showTaskCube(t)), indent = 2)
                  }
                  u <- putStrLn(s)
                } yield u

              case SubcommandType.GetPath =>
                for {
                  u <- ZIO.foreach_(targetJobs.map(_.absolutePath))(putStrLn(_))
                } yield u
              case SubcommandType.Describe =>
                for {
                  jobDescriptions <- ZIO.foreach(targetJobs)(_.describe)
                  u <- putStrLn(write(jobDescriptions, indent = 2))
                } yield u
              case SubcommandType.Run => new Run(targetJobs).run

              case SubcommandType.DryRun => new DryRun(targetJobs).run

              case SubcommandType.Invalidate =>
                val allRuns = File(s"${ctx.local.root}/.runs").children.map(_ / "jobs").toSeq
                val allRunJobs = allRuns.flatMap(_.lines).toSet[String].flatMap { s =>
                  parser.parseTask(fastparse.parse(s, Expressions.taskRef(_)).get.value)
                }
                val jobGraph = Graph.explore[Job](allRunJobs, _.dependentJobs)
                val jobsToBeInvalidated = Graph.explore[Job](targetJobs, jobGraph.outgoingNodes)
                val sortedJobsToBeInvalidated = jobsToBeInvalidated.topologicalSort
                for {
                  _ <- putStrLn(headerMessage)
                  _ <- putStrLn(
                    s"The following ${jobsToBeInvalidated.numNodes} jobs will be invalidated:"
                  )
                  _ <- ZIO.foreach_(sortedJobsToBeInvalidated)(_.printStatus(cli))
                  yes <- if (runtime.yes) ZIO.succeed(true) else cli.ask
                  u <-
                    Executor.run(sortedJobsToBeInvalidated)(_.invalidate as true) when yes
                } yield u

              case SubcommandType.Unlock =>
                for {
                  _ <- putStrLn(headerMessage)
                  _ <- putStrLn("The following jobs will be unlocked:")
                  _ <- ZIO.foreach_(targetJobs)(_.printStatus(cli))
                  yes <- if (runtime.yes) ZIO.succeed(true) else cli.ask
                  u <- Executor.run(targetJobs)(_.forceUnlock as true) when yes
                } yield u

              case SubcommandType.Remove =>
                for {
                  _ <- putStrLn(headerMessage)
                  _ <- putStrLn(s"The output of the following jobs will be removed:")
                  _ <- ZIO.foreach_(targetJobs)(_.printStatus(cli))
                  yes <- if (runtime.yes) ZIO.succeed(true) else cli.ask
                  u <-
                    Executor.run(targetJobs) { j =>
                      putStrLn(s"Removing job at ${j.absolutePath}") *> j.removeOutputs as true
                    } when yes
                } yield u

              case SubcommandType.Touch =>
                for {
                  _ <- putStrLn(headerMessage)
                  _ <- putStrLn("The following jobs will be marked as done:")
                  _ <- ZIO.foreach_(targetJobs)(_.printStatus(cli))
                  yes <- if (runtime.yes) ZIO.succeed(true) else cli.ask
                  u <- Executor.run(targetJobs)(_.markAsDone(cli)) when yes
                } yield u

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
