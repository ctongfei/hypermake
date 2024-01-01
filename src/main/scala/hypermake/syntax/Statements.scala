package hypermake.syntax

import scala.collection._
import fastparse._
import ast._
import hypermake.exception.ParsingException

/**
 * The statement grammar of Hypermake.
 * These are sensitive to newlines and indentations, following the Python format.
 */
class Statements(indent: Int) {
  import Expressions.{whitespace => _, _}

  implicit object whitespace extends fastparse.Whitespace {
    def apply(ctx: P[_]): P[Unit] = Lexical.wsComment(ctx)
  }

  def valDef[$: P] = P {
    identifier ~ "=" ~ expr
  } map { case (id, v) => ValDef(id, v) }

  def globalValDef[$: P] = P {
    Lexical.token("global") ~/ valDef
  } map { case ValDef(id, v) => GlobalValDef(id, v) }

  def funcDef[$: P] = P {
    Lexical.token("def") ~/ identifier ~ assignments ~ ("<-" ~ identifier ~ "=" ~ stringLiteral).? ~ scriptImpl
  } map { case (name, params, inputScript, impl) =>
    val (inputName, inputFilename) = inputScript.getOrElse(Identifier("NULL") -> StringLiteral("/dev/null"))
    FuncDef(name, params, inputName, inputFilename, impl)
  }

  def taskDef[_: P] = P {
    decoratorCalls ~
      Lexical.token("task") ~/ identifier ~ envModifier ~ assignments ~ ("->" ~ outputAssignments).? ~ impl
  } map { case (decorators, name, envMod, inputs, outputs, impl) =>
    TaskDef(decorators, name, envMod, inputs, outputs.getOrElse(Assignments(Seq())), impl)
  }

  def packageDef[_: P] = P {
    decoratorCalls ~
      Lexical.token("package") ~/ identifier ~ assignments ~ "->" ~ outputAssignment1 ~ scriptImpl
  } map { case (decorators, name, inputs, output, impl) => PackageDef(decorators, name, inputs, output, impl) }

  def planDef[_: P] = P {
    Lexical.token("plan") ~/ identifier ~ "=" ~ "{" ~/ taskRef.rep ~ "}"
  } map { case (name, taskRefs) => PlanDef(name, taskRefs) }

  def importFile[_: P] = P {
    Lexical.token("import") ~ Lexical.quotedString ~ (Lexical.token("as") ~ identifier).?
  } map { case (obj, name) => ImportFile(obj, name) }

  def importModule[_: P] = P {
    Lexical.token("import") ~ identifierPath ~ (Lexical.token("as") ~ identifier).?
  } map { case (path, name) => ImportModule(path, name) }

  def scriptLine[$: P]: P[String] = P {
    CharsWhile(_ != '\n').! | &("\n").!
  }

  def scriptBlock[$: P]: P[Verbatim] = P {
    "\n" ~~ deeper.flatMapX { i =>
      new Statements(i).scriptLine.repX(1, sep = "\n" + (" " * i))
    }
  } map { lines => Verbatim(lines.mkString("\n")) }

  def scriptImpl[$: P]: P[ScriptImpl] = P {
    ":" ~ scriptBlock
  } map ScriptImpl

  def funcCallImpl[_: P] = P {
    "=" ~ funcCall
  } map FuncCallImpl

  def impl[_: P]: P[Impl] = P { scriptImpl | funcCallImpl }

  def suiteImpl[_: P] = P {
    "{" ~ suite ~ "}"
  }

  def deeper[$: P]: P[Int] = P {
    (" " | "\t").repX(indent + 1).!.map(_.length)
  }

  def suite[$: P]: P[Seq[Statement]] = P {
    "\n" ~ deeper.flatMapX { i =>
      new Statements(i).stmt.rep(1, sep = ("\n" + ((" " * i) | ("\t" * i)))./)
    }
  }

  def stmt[$: P]: P[Statement] = P {
    valDef | globalValDef | funcDef | taskDef | packageDef | planDef | importFile | importModule
  }

  def top[$: P]: P[Seq[Statement]] = P {
    Lexical.wsComment ~ stmt.rep ~ End
  }

  private[hypermake] def parseFor[T](nonterminal: P[_] => P[T])(s: String): Parsed[T] =
    parse(s, nonterminal(_), verboseFailures = true)

  def syntacticParse(content: String): Seq[Statement] = {
    parse(content, top(_), verboseFailures = true) match {
      case Parsed.Success(a, _) => a
      case f: Parsed.Failure => throw ParsingException(f)
    }
  }

}

object Statements extends Statements(0)
