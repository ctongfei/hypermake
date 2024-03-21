package hypermake.util

/** Encapsulates the standard output and error as sinks.
  */
case class StdSinks(out: HSink[Byte], err: HSink[Byte])
