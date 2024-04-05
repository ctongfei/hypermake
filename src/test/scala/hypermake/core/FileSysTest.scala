package hypermake.core

import java.nio.file.{Files => JFiles, Path => JPath, Paths => JPaths}

import better.files.File
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import zio._
import zio.stream.ZSink

import hypermake.collection.PointedTensor
import hypermake.execution.RuntimeConfig
import hypermake.semantics.{Context, Definition, SemanticParser}
import hypermake.util.StdSinks

object FileSysGen {

  val pathComponent = for {
    len <- Gen.choose(1, 20)
    s <- Gen.listOfN(len, Gen.alphaNumChar).map(_.mkString)
  } yield s

  val genPath: Gen[JPath] = for {
    n <- Gen.choose(2, 6)
    p <- Gen.listOfN(n, pathComponent)
  } yield JPaths.get(p.mkString("/"))

  implicit val arbPath: Arbitrary[JPath] = Arbitrary(genPath)
  implicit val arbString: Arbitrary[String] = Arbitrary {
    for {
      n <- Gen.choose(0, 100)
      s <- Gen.listOfN(n, Gen.alphaNumChar).map(_.mkString)
    } yield s
  }

}

class FileSysLaws(local: FileSys, fs: FileSys)(implicit arbPath: Arbitrary[JPath], arbString: Arbitrary[String]) extends Laws {

  implicit val testSinks: StdSinks = StdSinks(
    ZSink.fromOutputStream(System.out),
    ZSink.fromOutputStream(System.err)
  )

  def fileSys = new DefaultRuleSet(
    "fileSys",
    None
//    "[write >> read]" -> Prop.forAll { (path: JPath, content: String) =>
//      val eff = for {
//        _ <- fs.mkdir(path.getParent.toString)
//        _ <- fs.write(path.toString, content)
//        c <- fs.read(path.toString)
//      } yield c == content
//      Runtime.default.unsafeRun(eff)
//    }
//    "[touch >> exists]" -> Prop.forAll { path: JPath =>
//      val eff = for {
//        _ <- fs.mkdir(path.getParent.toString)
//        _ <- fs.touch(path.toString)
//        e <- fs.exists(path.toString)
//      } yield e
//      Runtime.default.unsafeRun(eff)
//    },
//    "[touch >> delete >> !exists]" -> Prop.forAll { path: JPath =>
//      val eff = for {
//        _ <- fs.mkdir(path.getParent.toString)
//        _ <- fs.touch(path.toString)
//        a <- fs.exists(path.toString)
//        _ <- fs.delete(path.toString)
//        b <- fs.exists(path.toString)
//      } yield a && !b
//      Runtime.default.unsafeRun(eff)
//    },
//    "[write >> link >> read]" -> Prop.forAll { (src: JPath, dst: JPath, content: String) =>
//      val eff = for {
//        _ <- fs.mkdir(src.getParent.toString)
//        _ <- fs.write(src.toString, content)
//        _ <- fs.mkdir(dst.getParent.toString)
//        _ <- fs.link(src.toString, dst.toString)
//        c <- fs.read(dst.toString)
//      } yield c == content
//      Runtime.default.unsafeRun(eff)
//    }
  )

  def fileTransfer = new DefaultRuleSet(
    "fileTransfer",
    None,
    "[upload >> download]" -> Prop.forAll { (p: JPath, q: JPath, r: JPath, content: String) =>
      val eff = for {
        _ <- local.mkdir(p.getParent.toString)
        _ <- local.write(p.toString, content)
        _ <- fs.mkdir(q.getParent.toString)
        _ <- fs.upload(p.toString, q.toString)
        a <- fs.read(q.toString)
        _ <- local.mkdir(r.getParent.toString)
        _ <- fs.download(q.toString, r.toString)
        b <- local.read(r.toString)
      } yield {
        (a == content) && (b == content)
      }
      Runtime.default.unsafeRun(eff)
    }
  )
}

class FileSysTest extends AnyFunSuite with FunSuiteDiscipline with Checkers with BeforeAndAfterAll {

  import FileSysGen._

  implicit val runtime = RuntimeConfig.create(shell = "bash -eux")
  implicit val ctx = new Context()
  val tempDir = JFiles.createTempDirectory("hypermake-test")

  val predef =
    f"""
       |import ssh
       |local.root = "${tempDir.toString}"
       |object ssh_server = ssh.server(host="login.clsp.jhu.edu", root="/home/tongfei/sandbox/hypermake")
       |""".stripMargin

  val parser = new SemanticParser(ctx.root)
  parser.semanticParse(parser.readLinesToStmts(predef.split("\n")))
  val local = FileSys.local
  val ssh_server = FileSys("ssh_server")
  assert(local.root == tempDir.toString)
  implicit val stdSinks = StdSinks.default

  override def beforeAll(): Unit = {
    zio.Runtime.default.unsafeRun(
      FileSys.getTaskByName("ssh_server.setup").script.executeLocally(workDir = runtime.workDir)
    )
  }

  checkAll("FileSys laws", new FileSysLaws(local, ssh_server).fileSys)
  checkAll("FileTransfer laws", new FileSysLaws(local, ssh_server).fileTransfer)

  override def afterAll(): Unit = {
    File(tempDir).delete(swallowIOExceptions = true)
  }

}
