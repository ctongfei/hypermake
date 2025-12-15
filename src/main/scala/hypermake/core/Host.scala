package hypermake.core

import zio._

import hypermake.util._

trait Host {

  def fileSys: FileSys
  def execute(wd: String, command: String, args: Seq[String], envVars: Map[String, String])(implicit
      std: StdSinks
  ): HIO[ExitCode]

}

object Host {

  class Local(implicit ctx: Context) extends Host {
    def fileSys = FileSys.local

    def root = fileSys.root

    def execute(wd: String, command: String, args: Seq[String], envVars: Map[String, String])(implicit
        std: StdSinks
    ) = {
      fileSys.execute(wd, command, args, envVars)
    }
  }

}
