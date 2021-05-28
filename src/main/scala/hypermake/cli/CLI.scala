package hypermake.cli

import hypermake.core._
import hypermake.execution._
import hypermake.util._
import zio.stream.ZStream
import zio._

trait CLI {

  def initialize: HIO[Unit]
  def getSinks(job: Job): HIO[(HSink[Byte], HSink[Byte])]
  def update(job: Job, status: Status): HIO[Unit]


}
