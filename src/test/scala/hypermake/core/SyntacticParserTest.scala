package hypermake.core

import better.files.File
import hypermake.syntax.Expressions
import hypermake.syntax.Statements

object SyntacticParserTest extends App {

  val p = Statements.parseFor(Statements.scriptBlock(_))("\n  abc\n  def\n")

  val x = Statements.syntacticParse(File("src/test/resources/syntax-suite/task.hm").contentAsString)
  val bp = 0

}
