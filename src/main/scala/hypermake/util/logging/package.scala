package hypermake.util

import zio._
import zio.process._

import hypermake.core.Job
import hypermake.execution._
import hypermake.util.printing._

package object logging {

  def logCall(f: String, args: String*)(implicit runtime: RuntimeConfig): HIO[Unit] =
    ZIO.when(runtime.verbose)(zio.Console.printLineError(s"❯ Calling ${F(f)} ${A(args.mkString(" "))}")).unit

  def logExitCode(f: String, args: String*)(exitCode: ExitCode)(implicit runtime: RuntimeConfig): HIO[Unit] =
    ZIO.when(runtime.verbose) {
      for {
        _ <-
          zio.Console.printLineError(s"❮ Returned ${V(exitCode.code.toString)} from ${F(f)} ${A(args.mkString(" "))}")
      } yield ()
    }.unit

  def logTaskCall(j: Job)(implicit runtime: RuntimeConfig): HIO[Unit] = {
    ZIO.when(runtime.verbose) {
      val args = j.inputs.args.mapValuesE(_.absValue).map { case (k, v) => s"${A(k)}=${Vx(v)}" }.mkString("\n  ")
      val msg = s"❯❯ Calling task ${K(j.name)}(\n  $args\n)"
      zio.Console.printLineError(msg)
    }
  }.unit

}
