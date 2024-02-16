package hypermake.core

import better.files.File
import hypermake.execution.RuntimeConfig
import hypermake.semantics.{Context, SemanticParser}
import hypermake.syntax.{Expressions, Statements, syntacticParse}

object SyntacticParserTest extends App {

  implicit val runtime = RuntimeConfig.create()
  implicit val ctx = new Context
  val parser = new SemanticParser

  val obj = parser.semanticParseFile(
    File("src/test/resources/tutorial/glove.hm"),
    topLevel = true
  )
  val bp = 0

}
