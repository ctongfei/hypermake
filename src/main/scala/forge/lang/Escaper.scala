package forge.lang

import java.net.{URLDecoder, URLEncoder}


trait Escaper {

  def escape(s: String): String
  def unescape(s: String): String

}

object Escaper {

  object Percent extends Escaper {
    def escape(s: String) = URLEncoder.encode(s, "UTF-8")
    def unescape(s: String) = URLDecoder.decode(s, "UTF-8")
  }

  object C extends Escaper {
    val map = Map(
      '\u0007' -> 'a',
      '\b' -> 'b',
      "\u001b" -> 'e',
      '\f' -> 'f',
      '\n' -> 'n',
      '\r' -> 'r',
      '\t' -> 't',
      '\u000b' -> 'v',
      '\\' -> '\\',
      '\"' -> '\"',
      "\'" -> '\'',
      '?' -> '?',
    )
    def escape(s: String) = ???
    def unescape(s: String) = ???
  }

}
