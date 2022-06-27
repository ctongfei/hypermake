package hypermake.cli

import hypermake.core._
import hypermake.execution._
import hypermake.util._
import zio.stream.ZStream
import zio._


object CLI {

  trait Service {
    def setup: HIO[Unit]
    def println(s: String): HIO[Unit]
    def getSinks(job: Job): HIO[(HSink[Byte], HSink[Byte])]
    def update(job: Job, status: Status): HIO[Unit]
    def teardown: HIO[Unit]
    def ask: HIO[Boolean]
  }

}
