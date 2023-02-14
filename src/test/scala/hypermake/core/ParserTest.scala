package hypermake.core

import cats.syntax.all._
import better.files._
import fastparse._
import hypermake.collection._
import hypermake.execution._
import hypermake.semantics.{Context, SemanticParser}
import hypermake.syntax.SyntacticParser

object ParserTest extends App {

  import fastparse._
  val src = "task a:\n  r"
  val ast = parse(src, SyntacticParser.taskDef(_)).get.get

  implicit val rt = RuntimeContext.create()
  implicit val ctx = new Context()
  val workflow = new SemanticParser()
  workflow.semanticParse(File("src/test/resources/tutorial/decorators.hm"))


  val g = workflow.ctx.getPlan(Name("Run")).dependencyGraph
  val x = 0

}
