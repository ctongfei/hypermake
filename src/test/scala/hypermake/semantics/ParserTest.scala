package hypermake.semantics

import cats.syntax.all._
import better.files._
import fastparse._
import hypermake.core._
import hypermake.execution._
import hypermake.syntax.SyntacticParser

object ParserTest extends App {

  val file = File("src/test/resources/test.hm").lines.mkString("\n")

  val ast = SyntacticParser.syntacticParse(file)

  val z = 0

  implicit val env = RuntimeContext.create()
  implicit val ctx = new ParsingContext(env)
  val workflow = new SemanticParser()
  workflow.semanticParse(ast)


  val g = Resolver.resolvePlan(workflow.ctx.getPlan(Name("Run")))
  val tasks = g.topologicalSort
  val x = 0

}
