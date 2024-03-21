package hypermake.util

import java.io._
import java.nio.charset.{Charset, StandardCharsets}

import zio.stream._

object StandardStreams {

  final val out = new FileOutputStream(FileDescriptor.out)
  final val err = new FileOutputStream(FileDescriptor.err)

  val outSink = ZSink.fromOutputStream(out)
  val errSink = ZSink.fromOutputStream(err)

}
