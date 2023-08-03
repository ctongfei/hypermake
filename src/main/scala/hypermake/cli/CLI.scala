package hypermake.cli

import hypermake.core._
import hypermake.execution._
import hypermake.util._
import zio.stream.ZStream
import zio._


object CLI {

  trait Service {
    /** Creates a pair of sinks for the matser Hypermake thread. */
    def globalSinks: StdSinks

    /** Creates a pair of sinks for a specific job, potentially with the job metadata as prefix. */
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
