package hypermake.util

import zio.stream.ZSink

/** Encapsulates the standard output and error as sinks. */
case class StdSinks(out: HSink[Byte], err: HSink[Byte])

object StdSinks {
  val default: StdSinks = StdSinks(
    ZSink.fromOutputStream(System.out),
    ZSink.fromOutputStream(System.err)
  )

  val devNull: StdSinks = StdSinks(
    ZSink.drain.map(_ => 0),
    ZSink.drain.map(_ => 0)
  )
}
