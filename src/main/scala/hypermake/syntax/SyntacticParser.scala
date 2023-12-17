package hypermake.syntax

import fastparse._
import hypermake.exception._
import hypermake.util._
import hypermake.util.Escaper.Percent

object SyntacticParser {

  import Lexer._
  import fastparse.ScriptWhitespace._

  def ws(implicit prev: P[_]) = whitespace(prev)

  def string[_: P] = P {
    quotedString | unquotedString
  }

  def envModifier[_: P] = P {
    ("@" ~ identifier).?
  } map EnvModifier

  def keyValuePair[_: P] = P {
    string ~ ("=" ~ expr).?
  } map { case (k, ov) =>
    ov.fold[(String, Expr)]((k, StringLiteral(k, EnvModifier(None))))((v: Expr) => (k, v))
  }

  def key1[_: P] = P {
    string
  } map { sl => Key1(sl) }

  def keys[_: P] = P {
    "{" ~ string.rep(1, sep = ",") ~ "}"
  } map { sls => Keys(sls.toSet) }

  def star[_: P] = P {
    "*"
  } map { _ => Star() }

  def index1[_: P]: P[Index1] = P {
    identifier ~ ":" ~ key1
  } map { case (id, k) => Index1(id, k) }

  def indexN[_: P]: P[IndexN] = P {
    identifier ~ ":" ~ (key1 | keys | star) // | star
  } map {
    case (id, k: Key1)  => IndexN(id, Keys(Set(k.key)))
    case (id, ks: KeyN) => IndexN(id, ks)
    // case Star() => Index(Identifier("*"), Star())  // all other indices: e.g. [A: 3, B: 4, *]
  }

  def indices1[_: P]: P[Indices1] = P {
    ("[" ~ index1.rep(sep = ",") ~ "]").?
  } map { ois => Indices1(ois.getOrElse(Seq())) }

  def indicesN[_: P]: P[IndicesN] = P {
    ("[" ~ indexN.rep(sep = ",") ~ "]").?
  } map { ois => IndicesN(ois.getOrElse(Seq())) }

  def taskRef1[_: P]: P[TaskRef1] = P {
    identifier ~ indices1
  } map { case (id, indices) => TaskRef1(id, indices) }

  def taskRefN[_: P]: P[TaskRefN] = P {
    identifier ~ indicesN
  } map { case (id, indices) => TaskRefN(id, indices) }

  def valRef[_: P] = P {
    "$" ~ identifier ~ indices1
  } map { case (id, indices) => ValRef(id, indices) }

  def taskValRef1[_: P] = P {
    "$" ~ taskRef1 ~ "." ~ identifier
  } map { case (tr, id) => TaskOutputRef1(tr, id) }

  def taskValRefN[_: P] = P {
    "$" ~ taskRefN ~ "." ~ identifier
  } map { case (tr, id) => TaskOutputRefN(tr, id) }

  def stringLiteral[_: P]: P[StringLiteral] = P {
    string ~ envModifier
  } map { case (s, fsm) => StringLiteral(s, fsm) }

  def dictLiteral[_: P]: P[DictLiteral] = P {
    "{" ~ identifier ~ ":" ~
      (
        keyValuePair.rep(1, sep = ",").map(orderedMap)
          | inlineCommand.map(cmd => orderedMap(cmd.result().map(k => (Percent.escape(k), StringLiteral(k)))))
      ) ~ "}"
  } map { case (id, ps) => DictLiteral(id, ps) }

  def literal[_: P]: P[Literal] = stringLiteral | dictLiteral

  def expr[_: P]: P[Expr] = literal | taskValRef1 | taskValRefN | valRef

  def parameter[_: P] = P {
    identifier ~ envModifier
  } map { case (name, ofs) => Parameter(name, ofs) }

  def explicitAssignment[_: P] = P {
    parameter ~ "=" ~ expr
  } map { case (param, v) => ExplicitAssignment(param, v) }

  def refAssignment[_: P] = P {
    parameter ~ "=" ~ "$"
  } map RefAssignment

  def sameNameAssignment[_: P] = P {
    parameter
  } map { param => ExplicitAssignment(param, StringLiteral(param.name.name)) }

  def assignment[_: P]: P[Assignment] = explicitAssignment | refAssignment | sameNameAssignment

  def inputParamList[_: P] = P {
    identifier.rep(sep = ",")
  } map Identifiers

  def assignments[_: P] = P {
    "(" ~ assignment.rep(sep = ",") ~ ")"
  } map Assignments

  def funcCall[_: P] = P {
    identifier ~ assignments
  } map { case (funcName, inputs) => FuncCall(funcName, inputs) }

  def decoratorCall[_: P](indent: Int = 0) = P {
    indentation(indent) ~ "@" ~ funcCall ~ "\n"
  } map DecoratorCall

  def decoratorCalls[_: P](indent: Int = 0) =
    decoratorCall(indent).rep map (cs =>
      DecoratorCalls(cs.reverse)
    ) // reverse so that the first decorator is the outermost

  def funcCallImpl[_: P] = P {
    "=" ~ funcCall
  } map FuncCallImpl

  def impl[_: P](indent: Int): P[Impl] = scriptImpl(indent) | funcCallImpl

  def outputParamList[_: P] = identifier.map(x => Identifiers(Seq(x))) | ("(" ~ inputParamList ~ ")")

  def outputAssignments[_: P] = P {
    sameNameAssignment.map(a => Assignments(Seq(a))) | assignments
  }

  def outputAssignment1[_: P] = P {
    sameNameAssignment | explicitAssignment
  }

  def valDef[_: P](indent: Int = 0) = P {
    indentation(indent) ~ identifier ~ "=" ~ expr
  } map { case (id, v) => ValDef(id, v) }

  def globalValDef[_: P] = P {
    "global" ~ identifier ~ "=" ~ expr
  } map { case (id, v) => GlobalValDef(id, v) }

  def funcDef[_: P](indent: Int = 0) = P {
    indentation(indent) ~ "def" ~/ identifier ~ assignments ~ ("<-" ~ identifier ~ "=" ~ stringLiteral).? ~ scriptImpl(
      indent
    )
  } map { case (name, params, inputScript, impl) =>
    val (inputName, inputFilename) = inputScript.getOrElse(Identifier("NULL") -> StringLiteral("/dev/null"))
    FuncDef(name, params, inputName, inputFilename, impl)
  }

  def taskDef[_: P](indent: Int = 0) = P {
    decoratorCalls(indent) ~
      indentation(indent) ~ "task" ~ identifier ~ envModifier ~ assignments ~
      ("->" ~ outputAssignments).? ~ impl(indent)
  } map { case (decorators, name, envMod, inputs, outputs, impl) =>
    TaskDef(decorators, name, envMod, inputs, outputs.getOrElse(Assignments(Seq())), impl)
  }

//  def serviceDef[_: P] = P {
//    decoratorCalls ~
//      "service" ~/ identifier ~ envModifier ~ assignments ~ impl
//  } map { case (decorators, name, envMod, inputs, impl) => ServiceDef(decorators, name, envMod, inputs, impl) }

  def packageDef[_: P](indent: Int = 0) = P {
    decoratorCalls(indent) ~
      indentation(indent) ~ "package" ~/ identifier ~ assignments ~ "->" ~ outputAssignment1 ~ scriptImpl(indent)
  } map { case (decorators, name, inputs, output, impl) => PackageDef(decorators, name, inputs, output, impl) }

  def planDef[_: P](indent: Int = 0) = P {
    indentation(indent) ~ "plan" ~/ identifier ~ "=" ~ "{" ~/ taskRefN.rep ~ "}"
  } map { case (name, taskRefs) => PlanDef(name, taskRefs) }

  def moduleDef[_: P](indent: Int = 0) = P {
    indentation(indent) ~ "module" ~/ identifier ~ assignments ~ startIndentBlock ~ definition(indent + 2).rep
  } map { case (name, params, defs) => ModuleDef(name, params, defs) }

  def moduleInstantiation[_: P](indent: Int = 0) = P {
    indentation(indent) ~ identifier ~ "=" ~ identifier ~ assignments
  } map { case (name, module, params) => ModuleInstantiation(name, module, params) }

  def importStatement[_: P] = P {
    "import" ~ (string | moduleString)
  } map ImportStatement

  def definition[_: P](indent: Int): P[Def] =
    valDef(indent) | funcDef(indent) | taskDef(indent) | packageDef(indent) |
      planDef(indent) | moduleDef(indent) | moduleInstantiation(indent)

  def statement[_: P]: P[Statement] =
    definition(0) | globalValDef | importStatement

  def top[_: P]: P[Seq[Statement]] = P {
    ws ~ statement.rep ~ End
  }

  def syntacticParse(fileContent: String): Seq[Statement] = {
    parse(fileContent, top(_), verboseFailures = true) match {
      case Parsed.Success(a, _) => a
      case f: Parsed.Failure    => throw ParsingException(f)
    }
  }

}
