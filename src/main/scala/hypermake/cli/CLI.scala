package hypermake.cli

import hypermake.core._
import hypermake.execution._
import hypermake.util._
import zio.stream.ZStream
import zio._


object CLI {

  trait Service {
    def globalSinks: StdSinks
    def sinks(job: Job): StdSinks
    def setup: HIO[Unit]
    def println(s: String): HIO[Unit]
    def update(job: Job, status: Status): HIO[Unit]
    def show(job: Job, status: Status): HIO[String]
    def showInGraph(job: Job, status: Status): HIO[String]
    def teardown: HIO[Unit]
    def ask: HIO[Boolean]
  }

}
