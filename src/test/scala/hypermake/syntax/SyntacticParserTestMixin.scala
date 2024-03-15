package hypermake.syntax

import better.files.File
import org.scalatest.funsuite.AnyFunSuite

trait SyntacticParserTestMixin { self: AnyFunSuite =>

  def syntaxSuiteFile(name: String) =
    File(getClass.getClassLoader.getResource(s"syntax-suite/$name")).contentAsString

  def runTest(name: String) = try {
    val s = syntacticParse(syntaxSuiteFile(name))
  } catch {
    case e: Throwable => fail(e)
  }

}
