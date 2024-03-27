package hypermake.semantics

import better.files._
import hypermake.execution.RuntimeConfig
import org.scalatest.funsuite.AnyFunSuite

trait SemanticParserTestMixin { self: AnyFunSuite =>

  def semanticsSuiteFile(name: String) =
    File(getClass.getClassLoader.getResource(s"semantics-suite/$name"))

  def runTest(name: String) = try {
    implicit val runtime = RuntimeConfig.create()
    implicit val ctx = new Context()
    val parser = new SemanticParser(ctx.root)
    val obj = parser.semanticParseFile(semanticsSuiteFile(name), scope = ctx.root)
  } catch {
    case e: Throwable => fail(e)
  }

}
