package hypermake.testsuite

import better.files.File
import org.scalatest.funsuite.AnyFunSuite
import zio.{Chunk, Runtime, Unsafe, ZIOAppArgs, ZLayer}

import hypermake.Main

class PipelineTest extends AnyFunSuite {

  def run(filename: String, target: String): Unit = {
    val args = List(
      "-F",
      s"src/test/resources/pipeline-suite/$filename",
      "-I",
      "src/main/hypermake",
      "run",
      target,
      "-y"
    )
    println("Running: hypermake " + args.mkString(" "))
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(Main.run.provide(ZLayer.succeed(ZIOAppArgs(Chunk.fromIterable(args)))))
    }
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
