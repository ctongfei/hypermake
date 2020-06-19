package forge.lang

import scala.collection._
import fastparse._


object Lexer {
  import fastparse.NoWhitespace._
  import AST._

  val keywords = Set(
    "def", "task", "plan", "service", "package", "import", "stdin", "stdout", "stderr", "exitcode"
  )

  def comment[_: P] = P { "#" ~ CharsWhile(_ != '\n', 0) }

  def lowercase[_: P] = P { CharIn("a-z") }
  def uppercase[_: P] = P { CharIn("A-Z") }
  def letter[_: P] = P { lowercase | uppercase }
  def digit[_: P] = P { CharIn("0-9") }
  def specialChars[_: P] = P { CharIn("\\-.~") }

  def identifier[_: P] = P {
    ((letter | "_") ~ (letter | digit | "_").rep).!.filter(x => !keywords.contains(x))
  } map Identifier

  def escapeSequence[_: P] = P { "\\" ~ AnyChar }
  def stringChar[_: P](quote: Char) = P { CharsWhile(x => !s"\\\n\r$quote".contains(x)) }
  def stringItem[_: P](quote: Char) = P { stringChar(quote) | escapeSequence }
  def string0[_: P](quote: Char) = P { quote.toString ~ stringItem(quote).rep.! ~ quote.toString }
  def string[_: P] = P { string0('\'') | string0('\"') }

  def unquotedString[_: P] = P {
    (letter | digit | specialChars | "_").rep(1).!.filter(x => !keywords.contains(x))
  }

  def scriptLine[_: P]: P[String] = P {
    (("  " | "\t") ~ CharsWhile(_ != '\n').!) | &("\n").!
  }

  def script[_: P]: P[Script] = P {
    scriptLine.rep(min = 1, sep = "\n")
  } map { lines => Script(lines.mkString("\n")) }

  def scriptImpl[_: P] = P {
    ":\n" ~ script
  } map ScriptImpl

}

object Parser {
  import fastparse.ScriptWhitespace._
  import Lexer._
  import AST._

  def toMap[A, B](pairs: Iterable[(A, B)]): Map[A, B] =
    // maintains order in the keys
    mutable.LinkedHashMap.from(pairs)

  def stringLiteral[_: P] = P { string | unquotedString } map StringLiteral

  def keyValuePair[_: P] = P {
    stringLiteral ~ ("=" ~ value).?
  } map { case (k, ov) =>
    ov.fold[(StringLiteral, Value)]((k, k))(v => (k, v))
  }

  def keys[_: P] = P {
    stringLiteral.rep(1)
  } map { sls => Keys(sls.toSet) }

  def star[_: P] = P { "*" } map { _ => Star() }

  def index[_: P] = P {
    identifier ~ ":" ~ (keys | star)
  } map { case (id, ks) => Index(id, ks) }

  def indices[_: P] = P {
    ("[" ~ index.rep(sep = ",") ~ "]").?
  } map { is =>
    is.fold(Map[Identifier, KeySelector]())(s => toMap(s.map { case Index(axis, keys) => (axis, keys) }))
  }

  def taskRef[_: P]: P[TaskRef] = P {
    identifier ~ indices
  } map { case (id, indices) => TaskRef(id, indices) }

  def valRef[_: P] = P {
    "$" ~ identifier ~ ("@" ~ taskRef).?
  } map { case (id, taskRef) => ValRef(id, taskRef) }

  def dictLiteral[_: P]: P[DictLiteral] = P {
    "{" ~ identifier ~ ":" ~ keyValuePair.rep ~ "}"
  } map { case (id, ps) => DictLiteral(id, toMap(ps) ) }

  def literal[_: P]: P[Literal] = stringLiteral | dictLiteral

  def value[_: P]: P[Value] = literal | valRef

  def explicitAssignment[_: P] = P {
    identifier ~ "=" ~ value
  } map { case (id, v) => ExplicitAssignment(id, v) }

  def refAssignment[_: P] = P {
    identifier ~ "=" ~ "@"
  } map RefAssignment

  def sameNameAssignment[_: P] = P {
    identifier
  } map { id => ExplicitAssignment(id, StringLiteral(id.name)) }

  def assignment[_: P] = explicitAssignment | refAssignment | sameNameAssignment

  def identifiers[_: P] = P {
    identifier.rep(sep = ",")
  } map { inputs => inputs.toSet }

  def assignments[_: P] = P {
    assignment.rep(sep = ",")
  } map { inputs => toMap(inputs.map {
    case ExplicitAssignment(id, v) => (id, v)
    case RefAssignment(id) => (id, ValRef(id, None))
  })
  }

  def funcCall[_: P] = P {
    identifier ~ "(" ~ assignments ~ ")"
  } map { case (funcName, inputs) => FuncCall(funcName, inputs) }

  def decorator[_: P] = P {
    "@" ~ funcCall
  } map Decorator

  def funcCallImpl[_: P] = P {
    "=" ~ funcCall
  } map FuncCallImpl

  def impl[_: P]: P[Impl] = scriptImpl | funcCallImpl

  def outputIdentifiers[_: P] = identifier.map(x => Set(x)) | ("(" ~ identifiers ~ ")")

  def outputAssignments[_: P] = sameNameAssignment.map(a => Map(a.id -> a.value)) | ("(" ~ assignments ~ ")")

  def funcDef[_: P] = P {
    decorator.? ~
      "def" ~ identifier ~ "(" ~ identifiers ~ ")" ~ "->" ~ outputIdentifiers ~ impl
  } map { case (decorator, name, inputs, outputs, impl) => FuncDef(decorator, name, inputs, outputs, impl) }

  def taskDef[_: P] = P {
    decorator.? ~
      "task" ~ identifier ~ "(" ~ assignments ~ ")" ~ "->" ~ outputAssignments ~ impl
  } map { case (decorator, name, inputs, outputs, impl) => TaskDef(decorator, name, inputs, outputs, impl) }

  def serviceDef[_: P] = P {
    decorator.? ~
      "service" ~ identifier ~ "(" ~ assignments ~ ")" ~ "->" ~ outputAssignments ~ impl
  } map { case (decorator, name, inputs, outputs, impl) => ServiceDef(decorator, name, inputs, outputs, impl) }

  def packageDef[_: P] = P {
    decorator.? ~
      "package" ~ identifier ~ "(" ~ assignments ~ ")" ~ impl
  } map { case (decorator, name, inputs, impl) => PackageDef(decorator, name, inputs, impl) }

  def planDef[_: P] = P {
    "plan" ~ identifier ~ "=" ~ "{" ~ taskRef.rep ~ "}"
  } map { case (name, taskRefs) => PlanDef(name, taskRefs.toSet) }

  def decoratorDef[_: P] = P {
    "def" ~ identifier ~ "(" ~ identifiers ~ ")" ~ "(" ~ identifier ~ ")" ~ "->" ~ "(" ~ identifiers ~ ")" ~ impl
  } map { case (name, inputs, cmd, outputs, impl) => DecoratorDef(name, inputs, cmd, outputs, impl) }

  def importStmt[_: P] = P {
    "import" ~ stringLiteral
  } map ImportStmt

  def stmt[_: P] = explicitAssignment | funcDef | taskDef | serviceDef | packageDef | planDef | importStmt

  def top[_: P]: P[Seq[Stmt]] = P { stmt.rep ~ End }

}
