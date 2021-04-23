package hypermake.syntax

import fastparse._

object Lexer {
  import fastparse.NoWhitespace._

  val keywords = Set(
    "def", "task", "plan", "service", "package", "import", "under"
  )

  def comment[_: P] = P { "#" ~ CharsWhile(_ != '\n', 0) }

  def lowercase[_: P] = P { CharIn("a-z") }
  def uppercase[_: P] = P { CharIn("A-Z") }
  def letter[_: P] = P { lowercase | uppercase }
  def digit[_: P] = P { CharIn("0-9") }
  def specialChars[_: P] = P { CharIn("\\-.~") }

  def identifier[_: P] = P {
    ((letter | "_") ~ (letter | digit | "_").rep).!.filter(x => !keywords.contains(x))
  } map {s => Identifier(s)}

  def inlineCommand[_: P] = P {
    "`" ~ CharsWhile(_ != '`').! ~ "`"
  } map InlineCommand

  def escapeSequence[_: P] = P { "\\" ~ AnyChar }
  def stringChar[_: P](quote: Char) = P { CharsWhile(x => !s"\\\n\r$quote".contains(x)) }
  def stringItem[_: P](quote: Char) = P { stringChar(quote) | escapeSequence }
  def string0[_: P](quote: Char) = P { quote.toString ~ stringItem(quote).rep.! ~ quote.toString }
  def quotedString[_: P] = P { string0('\'') | string0('\"') }

  def unquotedString[_: P] = P {
    (letter | digit | specialChars | "_").rep(1).!.filter(x => !keywords.contains(x))
  }

  def pathString[_: P] = P {
    (letter | digit | CharPred(c => !(" *?<>\"".contains(c)))).rep(1).!
  }

  def scriptLine[_: P]: P[String] = P {
    (("  " | "\t") ~ CharsWhile(_ != '\n').!) | &("\n").!
  }

  def script[_: P]: P[Verbatim] = P {
    scriptLine.rep(min = 1, sep = "\n")
  } map { lines => Verbatim(lines.mkString("\n")) }

  def scriptImpl[_: P] = P {
    ":\n" ~ script
  } map ScriptImpl

}

