package hypermake

import scala.collection._

import fastparse.{P, Parsed, parse}

import hypermake.exception.ParsingException
import hypermake.syntax.Statements.top
import hypermake.syntax.ast._

package object syntax {

  private[hypermake] def parseFor[T](nonterminal: P[_] => P[T])(s: String): Parsed[T] =
    parse(s, nonterminal(_), verboseFailures = true)

  def syntacticParse(content: String): Seq[Statement] = {
    parse(content, top(_), verboseFailures = true) match {
      case Parsed.Success(a, _) => a
      case f: Parsed.Failure    => throw ParsingException(f)
    }
  }

}
