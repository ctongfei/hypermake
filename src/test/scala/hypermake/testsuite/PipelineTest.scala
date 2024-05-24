package hypermake.testsuite

import better.files.File
import org.scalatest.funsuite.AnyFunSuite

import hypermake.Main

class PipelineTest extends AnyFunSuite {

  def run(filename: String, target: String): Unit = {
    zio.Runtime.default.unsafeRun(
      Main.run(List(s"src/test/resources/pipeline-suite/$filename", "run", target, "-y"))
    )
  }

  test("interpreter") {
    run("interpreter.hm", "Run")
    assert(File("test-out/test_interpreter/default/stdout").contentAsString == "Hello, world!\n")
  }

  test("gcc") {
    run("gcc.hm", "Run")
    assert(File("test-out/print/default/stdout").contentAsString == "abcde\n")
  }

}
