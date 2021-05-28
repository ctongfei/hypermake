package hypermake.util

import zio.stream._

import java.io._
import java.nio.charset.{Charset, StandardCharsets}

object StandardStreams {

  class AutoFlushingOutputStream(os: OutputStream) extends FilterOutputStream(os) {
    override def write(b: Int): Unit = {
      if (b == '\n') os.flush()
      os.write(b)
    }

    override def write(b: Array[Byte], off: Int, len: Int): Unit = {
      if (b.indexOf('\n', off) != -1) os.flush()
      os.write(b, off, len)
    }

  }

  final val out = new AutoFlushingOutputStream(new FileOutputStream(FileDescriptor.out))
  final val err = new AutoFlushingOutputStream(new FileOutputStream(FileDescriptor.err))

  class PrefixedOutputStream(os: OutputStream, prefix: String) extends FilterOutputStream(os) {
    override def write(b: Array[Byte], off: Int, len: Int): Unit = os.synchronized {
      val lines = new String(b, off, len, StandardCharsets.UTF_8).split('\n')
      for (line <- lines) {
        os.write((prefix + line + "\n").getBytes(StandardCharsets.UTF_8))
      }
    }
  }

  val outSink = ZSink.fromOutputStream(out)
  val errSink = ZSink.fromOutputStream(err)

}
