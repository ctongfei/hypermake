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

  /**
   * Escapes a string for shell scripts (e.g. Bash).
   */
  object Shell extends Escaper {
    // Credit to https://stackoverflow.com/a/33949338/2990673
    def escape(s: String) = "'" + s.replace("'", "'\\''") + "'"
    def unescape(s: String) = {
      if (s.startsWith("'") && s.endsWith("'"))
        s.substring(1, s.length - 1).replace("'\\''", "'")
      else if (s.startsWith("\"") && s.endsWith("\""))
        C.unescape(s.substring(1, s.length - 1))
      else
        C.unescape(s)
    }
  }

}
