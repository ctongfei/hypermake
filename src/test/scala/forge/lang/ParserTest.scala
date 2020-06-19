package forge.lang

import better.files._
import fastparse._
import forge.core._
import forge.collection.Alphabet

object ParserTest extends App {

  val file = File("src/test/resources/test.fg").lines.mkString("\n")

  val ast = parse(file, Parser.top(_))
  val z = 0

  implicit val ctx = Context.create()
  val axes = for {
    stmts <- Semantics.readFileToStmts(File("src/test/resources/test.fg"))
    axes <- Semantics.axes(stmts)
  } yield axes

  val result = zio.Runtime.default.unsafeRun(axes)

  val x = 0

}
