package hypermake.syntax

import scala.collection._

import fastparse._

import hypermake.collection.Axis
import hypermake.syntax.ast._
import hypermake.util.Escaper.Percent
import hypermake.util.orderedMap

/**
 * The expression grammar of Hypermake.
 * These definitions ignore whitespaces and do not care about indentation.
 */
object Expressions {

  implicit object whitespace extends Whitespace {
    def apply(ctx: P[_]): P[Unit] = Lexical.wsComment(ctx)
  }

  def identifier[$: P] = Lexical.identifier

  def axisName[$: P] = identifier map { id =>
    AxisName(id.name)
  }

  def string[$: P] = P {
    Lexical.quotedString | Lexical.unquotedString
  }

  def identifierPath[$: P] = P {
    identifier.repX(min = 1, sep = ".")
  }.map(IdentifierPath)

  def fsModifier[$: P] = P {
    ("@" ~~ identifierPath).?
  } map FileSysModifier

  def stringLiteral[$: P]: P[StringLiteral] = P {
    Lexical.unquotedString | (Lexical.quotedString ~~ fsModifier)
  } map {
    case s: String => StringLiteral(s, FileSysModifier(None))
    case (s: String, fsm: FileSysModifier) =>
      StringLiteral(s, fsm)
  }

  def dictLiteral[$: P]: P[DictLiteral] = P {
    "{" ~ axisName ~ ":" ~
      (
        keyValuePair.rep(1).map(orderedMap)
          | Lexical.inlineCommand.map(cmd => orderedMap(cmd.result().map(k => (Percent.escape(k), StringLiteral(k)))))
      ) ~ "}"
  } map { case (a, ps) => DictLiteral(a, ps) }

  def literal[$: P]: P[Literal] = stringLiteral | dictLiteral

  def keyValuePair[$: P] = P {
    string ~ ("=" ~ expr).?
  } map { case (k, ov) =>
    ov.fold[(String, Expr)]((k, StringLiteral(k, FileSysModifier(None))))((v: Expr) => (k, v))
  }

  def keys[$: P] = P {
    string.rep(1)
  } map { sls => Keys(sls.toSet) }

  def star[$: P] = P {
    "*"
  } map { _ => Star() }

  def index[$: P]: P[AxisIndex] = P {
    identifier ~ ":" ~ (keys | star) // | star
  } map { case (id, ks: KeyN) =>
    AxisIndex(id, ks)
  }

  def indices[$: P]: P[AxisIndices] = P {
    ("[" ~ index.rep(sep = ",") ~ "]").?
  } map { ois => AxisIndices(ois.getOrElse(Seq())) }

  def taskRef[$: P]: P[TaskRef] = P {
    identifierPath ~ indices
  } map { case (id, indices) => TaskRef(id, indices) }

  def outputRef[$: P]: P[OutputRef] = P {
    "." ~~ identifier
  } map OutputRef

  def valRef0[$: P]: P[ValRef] = P {
    identifierPath ~ (indices ~ outputRef).?
  } map {
    case (id, Some((indices, oor))) => ValRef(id, indices, Some(oor))
    case (id, None)                 => ValRef(id, AxisIndices(Seq()), None)
  }

  def valRef[$: P]: P[ValRef] = P { "$" ~~ valRef0 }

  def expr[$: P]: P[Expr] = literal | valRef

  def parameter[$: P] = P {
    identifier ~ fsModifier
  } map { case (name, ofs) => Parameter(name, ofs) }

  def explicitAssignment[$: P] = P {
    parameter ~ "=" ~ expr
  } map { case (param, v) => ExplicitAssignment(param, v) }

  def refAssignment[$: P] = P {
    parameter ~ "=" ~ "$"
  } map RefAssignment

  // TODO: only appears at output positions
  def sameNameAssignment[$: P] = P {
    parameter
  } map { param => ExplicitAssignment(param, StringLiteral(param.name.name)) }

  def assignment[$: P]: P[Assignment] = explicitAssignment | refAssignment | sameNameAssignment

  def inputParamList[$: P] = P {
    identifier.rep(sep = ",")
  } map Identifiers

  def assignments[$: P] = P {
    "(" ~ assignment.rep(sep = ",") ~ ")"
  } map Assignments

  def call[$: P] = P {
    identifierPath ~ assignments
  } map { case (funcName, inputs) => Call(funcName, inputs) }

  def decoratorCall[$: P] = P {
    "@" ~ identifierPath ~ assignments.?
  } map { case (cls, args) =>
    Decoration(cls, args)
  }

  def decoratorCalls[$: P] =
    decoratorCall.rep map (cs => DecoratorCalls(cs.reverse)) // reverse so that the first decorator is the outermost

  def funcCallImpl[$: P] = P {
    "=" ~ call
  } map FuncCallImpl

  def outputParamList[$: P] =
    identifier.map(x => Identifiers(Seq(x))) | ("(" ~ inputParamList ~ ")")

  def outputAssignments[$: P] = P {
    sameNameAssignment.map(a => Assignments(Seq(a))) | assignments
  }

  def outputAssignment1[$: P] = P {
    sameNameAssignment | explicitAssignment
  }
}
