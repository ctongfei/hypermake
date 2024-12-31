package hypermake.util

import zio._

import hypermake.execution._
import hypermake.util.printing._

package object logging {

  def logCall(f: String, args: String*)(implicit runtime: RuntimeConfig): HIO[Unit] =
    ZIO.when(runtime.verbose)(zio.console.putStrLnErr(s"[${K(f)} ${args.mkString(" ")}]"))

}
