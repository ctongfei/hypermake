package hypermake.core

import cats.syntax.all._
import better.files._
import fastparse._
import hypermake.collection._
import hypermake.execution._
import hypermake.semantics.{Context, SemanticParser}
import hypermake.syntax.Statements

object ParserTest extends App {

  import fastparse._

  implicit val rt = RuntimeConfig.create()
  implicit val ctx = new Context()
  val parser = new SemanticParser()
  val obj = parser.semanticParseFile(File("src/test/resources/test-sequential.hm"), topLevel = true)

  val g = obj.plans("Run").dependencyGraph
  val x = 0

}
