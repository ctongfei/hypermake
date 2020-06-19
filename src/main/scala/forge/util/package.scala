package forge

import fastparse._
import zio._

import forge.exception._

package object util {

  implicit class ParsedExtensions[A](val p: Parsed[A]) extends AnyVal {
    def asTask: Task[A] = p match {
      case Parsed.Success(a, _) => ZIO.succeed(a)
      case f @ Parsed.Failure(_, _, _) => ZIO.fail(ParsingException(f))
    }
  }

}
