package hypermake.util

import zio._
import zio.process._

import hypermake.core.Job
import hypermake.execution._
import hypermake.util.printing._

package object logging {

  def logCall(f: String, args: String*)(implicit runtime: RuntimeConfig): HIO[Unit] =
    ZIO.when(runtime.verbose)(zio.console.putStrLnErr(s"❯ Calling ${K(f)} ${Vx(args.mkString(" "))}"))

  def logExitCode(f: String, args: String*)(process: Process)(implicit runtime: RuntimeConfig): HIO[Unit] =
    ZIO.when(runtime.verbose) {
      for {
        exitCode <- process.exitCode
        _ <- zio.console.putStrLnErr(s"❮ Returned ${V(exitCode.code.toString)} from ${K(f)} ${Vx(args.mkString(" "))}")
      } yield ()
    }

  def logTaskCall(j: Job)(implicit runtime: RuntimeConfig): HIO[Unit] = {
    ZIO.when(runtime.verbose) {
      val args = j.inputs.args.mapValuesE(_.absValue).map { case (k, v) => s"${K(k)}=${V(v)}" }.mkString("\n  ")
      val msg = s"❯❯ Calling task ${K(j.name)}(\n  $args\n)"
      zio.console.putStrLnErr(msg)
    }
  }

}
