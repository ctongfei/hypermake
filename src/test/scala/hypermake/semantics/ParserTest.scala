package hypermake.semantics

import cats.syntax.all._
import better.files._
import fastparse._
import hypermake.core._
import hypermake.execution._
import hypermake.syntax.SyntacticParser

object ParserTest extends App {

//  import fastparse._
//  val ast = parse("""task t0() -> out:
//      a
//    """, SyntacticParser.taskDef(_)).get.get

  implicit val rt = RuntimeContext.create()
  implicit val ctx = new ParsingContext()
  val workflow = new SemanticParser()
  workflow.semanticParse(File("src/test/resources/test-sequential.hm"))


  val g = Resolver.resolvePlan(workflow.ctx.getPlan(Name("Run")))
  val tasks = g.topologicalSort
  val x = 0

}
