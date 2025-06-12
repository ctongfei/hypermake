package hypermake.util

import hypermake.core.Job
import zio._
import hypermake.execution._
import hypermake.util.printing._

package object logging {

  def logCall(f: String, args: String*)(implicit runtime: RuntimeConfig): HIO[Unit] =
    ZIO.when(runtime.verbose)(zio.console.putStrLnErr(s"❯ Calling (${K(f)} ${Vx(args.mkString(" "))})"))

  def logResult(f: String, result: String)(implicit runtime: RuntimeConfig): HIO[Unit] =
    ZIO.when(runtime.verbose)(zio.console.putStrLnErr(s"❯ ${K(f)} returns ${V(result)}"))

  def logTaskCall(j: Job)(implicit runtime: RuntimeConfig): HIO[Unit] = {
    val args = j.inputs.args.mapValuesE(_.absValue).map { case (k, v) => s"${K(k)}=${V(v)}" }.mkString("\n  ")
    val msg = s"❯ Calling task ${K(j.name)}(\n  $args\n)"
    ZIO.when(runtime.verbose)(zio.console.putStrLnErr(msg))
  }

}
