package hypermake.core

import cats.syntax.all._
import better.files._
import fastparse._
import hypermake.collection._
import hypermake.execution._
import hypermake.semantics.{SymbolTable, SemanticParser}
import hypermake.syntax.SyntacticParser

object ParserTest extends App {

  import fastparse._
  val ast = parse(
    """task a() -> ():
      |  print
      |""".stripMargin, SyntacticParser.taskDef(_)).get.get

  implicit val rt = RuntimeContext.create()
  implicit val ctx = new SymbolTable()
  val workflow = new SemanticParser()
  workflow.semanticParse(File("src/test/resources/test.hm"))


  val g = workflow.ctx.getPlan(Name("Run")).dependencyGraph
  val x = 0

}
