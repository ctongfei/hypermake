package hypermake.syntax

import better.files.File
import org.scalatest.funsuite.AnyFunSuite

class SyntacticParserTest extends AnyFunSuite {

  def syntaxSuiteFile(name: String) =
    File(getClass.getClassLoader.getResource(s"syntax-suite/$name")).contentAsString

  def runTest(name: String) = try {
    val s = syntacticParse(syntaxSuiteFile(name))
  } catch {
    case e: Throwable => fail(e)
  }

  test("Syntax of literals") { runTest("literal.hm") }
  test("Syntax of references") { runTest("ref.hm") }
  test("Syntax of tasks") { runTest("task.hm") }
  test("Syntax of objects") { runTest("object.hm") }
  test("Syntax of import statements") { runTest("import.hm") }

}
