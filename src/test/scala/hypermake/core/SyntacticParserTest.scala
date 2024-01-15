package hypermake.core

import better.files.File
import hypermake.syntax.{Expressions, Statements, syntacticParse}

object SyntacticParserTest extends App {

  val x = syntacticParse(
    File("src/test/resources/syntax-suite/object.hm").lines.filterNot(_.trim == "").mkString("\n")
  )
  val bp = 0

}
