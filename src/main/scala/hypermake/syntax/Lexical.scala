package hypermake.syntax

import hypermake.syntax.ast._

/** The lexical part of the grammar of Hypermake. The following non-terminal definitions are
  * sensitive to whitespace.
  */
object Lexical {

  import fastparse._
  import fastparse.NoWhitespace._

  val keywords = Set(
    "as",
    "import",
    "def",
    "task",
    "plan",
    "service",
    "package",
    "class",
    "object"
  )

  def token[$: P](s: String) = s ~ !(letter | digit | "_")

  def comment[$: P] = P { "#" ~ CharsWhile(_ != '\n', 0) }

  def wsComment[$: P] = P {
    (CharsWhileIn(" \n") | comment | "\\\n").rep
  }

  def noNewlineWsComment[$: P] = P {
    (CharsWhileIn(" ") | comment | "\\\n").rep
  }

  def lowercase[$: P] = P(CharIn("a-z"))
  def uppercase[$: P] = P(CharIn("A-Z"))
  def letter[$: P] = P(lowercase | uppercase)
  def digit[$: P] = P(CharIn("0-9"))

  def specialChars[$: P] = P {
    CharIn("\\-.~")
  }

  def identifier[$: P] = P {
    ((letter | "_") ~ (letter | digit | "_").rep).!.filter(x => !keywords.contains(x))
  } map Identifier.apply

  def inlineCommand[$: P] = P {
    "`" ~ CharsWhile(_ != '`').! ~ "`"
  } map InlineCommand

  def escapeSequence[$: P] = P {
    "\\" ~ AnyChar
  }

  def stringChar[$: P](quote: Char) = P {
    CharsWhile(x => !s"\\\n\r$quote".contains(x))
  }

  def stringItem[$: P](quote: Char) = P {
    stringChar(quote) | escapeSequence
  }

  def string0[$: P](quote: Char) = P {
    quote.toString ~ stringItem(quote).rep.! ~ quote.toString
  }

  def quotedString[$: P] = P {
    string0('\'') | string0('\"')
  }

  def unquotedString[$: P] = P {
    (letter | digit | specialChars | "_").rep(1).!.filter(x => !keywords.contains(x))
  }

  def pathString[$: P] = P {
    (letter | digit | CharPred(c => !(" *?<>\"".contains(c)))).rep(1).!
  }

}
