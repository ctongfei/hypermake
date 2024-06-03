package hypermake.core

import java.nio.file.{Path => JPath, Paths => JPaths}

import better.files.File
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import zio._
import zio.stream.ZSink

import hypermake.execution.RuntimeConfig
import hypermake.semantics.{Context, SemanticParser}
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
    None,
    "read" -> Prop.forAll { (path: JPath, content: String) =>
      val eff = for {
        _ <- fs.mkdir(path.getParent.toString)
        _ <- fs.write(path.toString, content)
        c <- fs.read(path.toString)
      } yield c == content
      Runtime.default.unsafeRun(eff)
    },
    "mkdir" -> Prop.forAll { path: JPath =>
      val eff = for {
        _ <- fs.mkdir(path.toString)
        e <- fs.exists(path.toString)
      } yield e
      Runtime.default.unsafeRun(eff)
    },
    "removeEmptyDir" -> Prop.forAll { path: JPath =>
      val eff = for {
        _ <- fs.mkdir(path.toString)
        a <- fs.exists(path.toString)
        _ <- fs.remove(path.toString)
        b <- fs.exists(path.toString)
      } yield a && !b
      Runtime.default.unsafeRun(eff)
    },
    "removeNonEmptyDir" -> Prop.forAll { (path: JPath, a: String, b: String) =>
      val eff = for {
        _ <- fs.mkdir(path.toString)
        _ <- fs.write(s"${path.toString}${fs./}a", a)
        _ <- fs.write(s"${path.toString}${fs./}b", b)
        a <- fs.exists(path.toString)
        _ <- fs.remove(path.toString)
        b <- fs.exists(path.toString)
      } yield a && !b
      Runtime.default.unsafeRun(eff)
    },
    "deleteFile" -> Prop.forAll { path: JPath =>
      val eff = for {
        _ <- fs.mkdir(path.getParent.toString)
        _ <- fs.touch(path.toString)
        a <- fs.exists(path.toString)
        _ <- fs.remove(path.toString)
        b <- fs.exists(path.toString)
      } yield a && !b
      Runtime.default.unsafeRun(eff)
    },
    "linkFile" -> Prop.forAll { (src: JPath, dst: JPath, content: String) =>
      val eff = for {
        _ <- fs.mkdir(src.getParent.toString)
        _ <- fs.write(src.toString, content)
        _ <- fs.mkdir(dst.getParent.toString)
        _ <- fs.link(src.toString, dst.toString)
        c <- fs.read(dst.toString)
      } yield c == content
      Runtime.default.unsafeRun(eff)
    },
    "linkDir" -> Prop.forAll { (src: JPath, dst: JPath, a: String, b: String) =>
      val eff = for {
        _ <- fs.mkdir(src.toString)
        _ <- fs.write(s"${src.toString}${fs./}a", a)
        _ <- fs.write(s"${src.toString}${fs./}b", b)
        _ <- fs.mkdir(dst.toString)
        _ <- fs.link(src.toString, dst.toString)
        c <- fs.read(s"${dst.toString}${fs./}a")
        d <- fs.read(s"${dst.toString}${fs./}b")
      } yield (a == c) && (b == d)
      Runtime.default.unsafeRun(eff)
    }
  )

  def fileTransfer = new DefaultRuleSet(
    "fileTransfer",
    None,
    "uploadThenDownloadFile" -> Prop.forAll { (p: JPath, q: JPath, r: JPath, content: String) =>
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
    },
    "uploadThenDownloadDir" -> Prop.forAll { (p: JPath, q: JPath, r: JPath, a: String, b: String) =>
      val eff = for {
        _ <- local.mkdir(p.toString)
        _ <- local.write(s"${p.toString}${local./}a", a)
        _ <- local.write(s"${p.toString}${local./}b", b)
        _ <- fs.mkdir(q.getParent.toString)
        _ <- fs.upload(p.toString, q.toString)
        _ <- local.mkdir(r.getParent.toString)
        _ <- fs.download(q.toString, r.toString)
        c <- local.read(s"${r.toString}${local./}a")
        d <- local.read(s"${r.toString}${local./}b")
      } yield {
        (a == c) && (b == d)
      }
      Runtime.default.unsafeRun(eff)
    }
  )
}

class FileSysTest extends AnyFunSuite with FunSuiteDiscipline with Checkers with BeforeAndAfterAll {

  import FileSysGen._

  implicit val config: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 3, maxDiscardedFactor = 0.1)
  implicit val runtime = RuntimeConfig.create(shell = "bash -eux")
  implicit val ctx = new Context()

  val parser = new SemanticParser(ctx.root)
  parser.semanticParse(parser.readFileToStmts(File(getClass.getClassLoader.getResource("test-filesys.hm"))))
  val local = FileSys.local
  val s3 = FileSys("my_s3")
  val asb = FileSys("my_asb")
  val sftp = FileSys("my_sftp")
  implicit val stdSinks: StdSinks = StdSinks.default

//  override def beforeAll(): Unit = {
//    val eff = ctx.root.tasks("my_sftp.setup").default.script.executeLocally(runtime.workDir)
//    Runtime.default.unsafeRun(eff)
//  }

//  checkAll("local", new FileSysLaws(local, local).fileSys)
//  checkAll("local", new FileSysLaws(local, local).fileTransfer)
  checkAll("aws.s3", new FileSysLaws(local, s3).fileSys)
  checkAll("aws.s3", new FileSysLaws(local, s3).fileTransfer)
//  checkAll("az.blob_storage", new FileSysLaws(local, asb).fileSys)
//  checkAll("az.blob_storage", new FileSysLaws(local, asb).fileTransfer)

}
