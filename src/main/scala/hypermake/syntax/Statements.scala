package hypermake.syntax

import scala.collection._
import fastparse._
import ast._

/** The statement grammar of Hypermake. These are sensitive to newlines and indentations, following
  * the Python format.
  */
class Statements(indent: Int) {

  import Expressions.{whitespace => _, _}

  implicit object whitespace extends fastparse.Whitespace {
    def apply(ctx: P[_]): P[Unit] = Lexical.wsComment(ctx)
  }

  def valDef[$: P] = P {
    identifierPath ~ "=" ~ expr
  } map { case (id, v) => ValDef(id, v) }

  def funcDef[$: P] = P {
    Lexical.token("def") ~/ identifier ~ assignments ~ scriptImpl
  } map { case (name, params, impl) =>
    FuncDef(name, params, impl)
  }

  def taskDef[$: P] = P {
    decoratorCalls ~
      Lexical.token(
        "task"
      ) ~/ identifier ~ fsModifier ~ assignments.? ~ ("->" ~ outputAssignments).? ~ impl
  } map { case (decorators, name, fsModifier, inputs, outputs, impl) =>
    TaskDef(
      decorators,
      name,
      fsModifier,
      inputs.getOrElse(Assignments(Seq())),
      outputs.getOrElse(Assignments(Seq())),
      impl
    )
  }

  def packageDef[$: P] = P {
    decoratorCalls ~
      Lexical.token("package") ~/ identifier ~ assignments.? ~ "->" ~ outputAssignment1 ~ scriptImpl
  } map { case (decorators, name, inputs, output, impl) =>
    PackageDef(decorators, name, inputs.getOrElse(Assignments(Seq())), output, impl)
  }

  def planDef[$: P] = P {
    Lexical.token("plan") ~/ identifier ~ "=" ~ "{" ~/ taskRef.rep ~ "}"
  } map { case (name, taskRefs) => PlanDef(name, taskRefs) }

  def defsImpl[$: P]: P[MembersImpl] = P {
    ":" ~~ Lexical.noNewlineWsComment ~~ suite
  } map MembersImpl

  def instantiationImpl[$: P] = P {
    "=" ~ call
  } map InstantiationImpl

  def moduleImpl[$: P]: P[ObjectImpl] = P {
    defsImpl | instantiationImpl
  }

  def classDef[$: P] = P {
    Lexical.token("class") ~/ identifier ~ assignments ~ moduleImpl
  } map { case (name, inputs, impl) => ClassDef(name, inputs, impl) }

  def objectDef[$: P] = P {
    Lexical.token("object") ~/ identifier ~ moduleImpl
  } map { case (name, impl) => ObjectDef(name, impl) }

  def importFile[$: P] = P {
    Lexical.token("import") ~ Lexical.quotedString ~ (Lexical.token("as") ~ identifier).?
  } map { case (obj, name) => ImportFile(obj, name) }

  def importObject[$: P] = P {
    Lexical.token("import") ~ identifierPath ~ (Lexical.token("as") ~ identifier).?
  } map { case (path, name) => ImportObject(path, name) }

  def scriptLine[$: P]: P[String] = P {
    CharsWhile(_ != '\n').! | &("\n").!
  }

  def scriptBlock[$: P]: P[Verbatim] = P {
    "\n" ~~ deeper.flatMapX { i =>
      new Statements(i).scriptLine.repX(1, sep = "\n" + (" " * i))
    }
  } map { lines => Verbatim(lines.mkString("\n")) }

  def scriptImpl[$: P]: P[ScriptImpl] = P {
    ":" ~~ Lexical.noNewlineWsComment ~~ scriptBlock
  } map ScriptImpl

  def funcCallImpl[$: P] = P {
    "=" ~ call
  } map FuncCallImpl

  def impl[$: P]: P[TaskImpl] = P {
    scriptImpl | funcCallImpl
  }

  // TODO: suiteImpl of a task
  // task t(a) -> b = {
  //   task f(a) -> c =
  //   task g(c) -> b
  // }

  def suiteImpl[$: P] = P {
    "{" ~ suite ~ "}"
  }

  def deeper[$: P]: P[Int] = P {
    (" " | "\t").repX(indent + 1).!.map(_.length)
  }

  def suite[$: P]: P[Seq[Def]] = P {
    "\n" ~~ deeper.flatMapX { i =>
      new Statements(i).definition.repX(1, sep = ("\n" + (" " * i))./)
    }
  }

  def definition[$: P]: P[Def] = P {
    valDef | funcDef | taskDef | packageDef | planDef | classDef | objectDef
  }

  def stmt[$: P]: P[Statement] = P {
    definition | importFile | importObject
  }

  def top[$: P]: P[Seq[Statement]] = P {
    Lexical.wsComment ~ stmt.rep ~ End
  }

}

object Statements extends Statements(0)
