package hypermake.util

import zio.stream._

import java.io._
import java.nio.charset.{Charset, StandardCharsets}

object StandardStreams {

  final val out = new FileOutputStream(FileDescriptor.out)
  final val err = new FileOutputStream(FileDescriptor.err)

  class PrefixedOutputStream(os: OutputStream, prefix: String) extends FilterOutputStream(os) {
    private[this] var atStartOfLine: Boolean = true
    override def write(b: Array[Byte], off: Int, len: Int): Unit = os.synchronized {
      val lines = new String(b, off, len, StandardCharsets.UTF_8).split("(?<=[\\r\\n])")
      // split that includes concluding delimiters with positive lookbehind regex
      for (line <- lines) {
        if (atStartOfLine) os.write(prefix.getBytes(StandardCharsets.UTF_8))
        os.write(line.getBytes(StandardCharsets.UTF_8))
        atStartOfLine = line.endsWith("\n") || line.endsWith("\r")
        os.flush()
      }
    }
  }

  val outSink = ZSink.fromOutputStream(out)
  val errSink = ZSink.fromOutputStream(err)

}
