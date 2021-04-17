package hypermake.util

import java.net.{URLDecoder, URLEncoder}


trait Escaper {

  def escape(s: String): String
  def unescape(s: String): String

}

object Escaper {

  /**
   * Escapes strings using Percent encoding (a.k.a. URL encoding).
   */
  object Percent extends Escaper {
    def escape(s: String) = URLEncoder.encode(s, "UTF-8")
    def unescape(s: String) = URLDecoder.decode(s, "UTF-8")
  }

  /**
   * Escapes strings using C/C++ standard.
   */
  object C extends Escaper {
    val escapedChars: Map[Char, Char] = Map(
      '\u0007' -> 'a',
      '\b' -> 'b',
      '\u001b' -> 'e',
      '\f' -> 'f',
      '\n' -> 'n',
      '\r' -> 'r',
      '\t' -> 't',
      '\u000b' -> 'v',
      '\\' -> '\\',
      '\"' -> '\"',
      '\'' -> '\'',
    )
    def escape(s: String) = {
      val sb = new StringBuilder()
      for (c <- s)
        sb append (if (escapedChars contains c) "\\" + c else c.toString)
      sb.toString()
    }

    def unescape(s: String) = StringContext.processEscapes(s)
  }

}
