package hypermake

import zio._
import zio.console._

import hypermake.cli._
import hypermake.collection.Graph
import hypermake.core._
import hypermake.execution._
import hypermake.util._

abstract class Subcommand(implicit ctx: Context, cli: CLI.Service) {
  implicit val runtime: RuntimeConfig = ctx.runtime
  implicit val std: StdSinks = cli.globalSinks

  def run: HIO[Unit]
}

abstract class RunTarget(targetJobs: Seq[Job])(implicit ctx: Context, cli: CLI.Service) extends Subcommand()(ctx, cli) {
  def jobGraph = Graph.exploreBidirectionally[Job](
    targetJobs,
    prev = j => j.dependentJobs ++ j.services.map(_.start),
    next = j => j.services.map(_.stop)
  )
}

class Run(targetJobs: Seq[Job])(implicit ctx: Context, cli: CLI.Service) extends RunTarget(targetJobs)(ctx, cli) {
  def run = {
    val graph = jobGraph
    val sortedJobs = graph.topologicalSort.toIndexedSeq
    val ephemeralJobs = sortedJobs.filter(_.ephemeral)

    for {
      _ <- putStrLn(headerMessage)
      _ <- putStrLn(
        s"The following ${graph.numNodes} jobs are implied in the given target:"
      )
      s <- graph.toStringIfAcyclic(_.statusString(cli))
      _ <- putStrLn(s)
      yes <- if (runtime.yes) ZIO.succeed(true) else cli.ask
      u <- (Executor.recordJobsRun(sortedJobs, cli) *> Executor.runDAG(graph, cli)).when(yes)
      _ <- ZIO.foreach_(ephemeralJobs)(_.removeOutputs)
    } yield u
  }
}

class DryRun(targetJobs: Seq[Job])(implicit ctx: Context, cli: CLI.Service) extends RunTarget(targetJobs)(ctx, cli) {
  def run = {
    val graph = jobGraph
    for {
      _ <- putStrLn(headerMessage)
      _ <- putStrLn(
        s"The following ${jobGraph.numNodes} jobs are implied in the given target:"
      )
      s <- jobGraph.toStringIfAcyclic(_.statusString(cli))
      u <- putStrLn(s)
    } yield u
  }
}
