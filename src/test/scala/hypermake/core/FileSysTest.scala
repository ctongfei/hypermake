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
import zio.console.putStrLn
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

class FileSysLaws(local: FileSys, remote: FileSys)(implicit arbPath: Arbitrary[JPath], arbString: Arbitrary[String]) extends Laws {

  implicit val testSinks: StdSinks = StdSinks(
    ZSink.fromOutputStream(System.out),
    ZSink.fromOutputStream(System.err)
  )

  def fileSys = new DefaultRuleSet(
    "fileSys",
    None,
    "read" -> Prop.forAll { (path: JPath, content: String) =>
      val eff = for {
        _ <- remote.mkdir(path.getParent.toString)
        _ <- remote.write(path.toString, content)
        c <- remote.read(path.toString)
      } yield c == content
      Runtime.default.unsafeRun(eff)
    },
    "mkdir" -> Prop.forAll { path: JPath =>
      val eff = for {
        _ <- remote.mkdir(path.toString)
        e <- remote.exists(path.toString)
      } yield e
      Runtime.default.unsafeRun(eff)
    },
    "removeEmptyDir" -> Prop.forAll { path: JPath =>
      val eff = for {
        _ <- remote.mkdir(path.toString)
        a <- remote.exists(path.toString)
        _ <- remote.remove(path.toString)
        b <- remote.exists(path.toString)
      } yield a && !b
      Runtime.default.unsafeRun(eff)
    },
    "removeNonEmptyDir" -> Prop.forAll { (path: JPath, a: String, b: String) =>
      val eff = for {
        _ <- remote.mkdir(path.toString)
        _ <- remote.write(s"${path.toString}${remote./}a", a)
        _ <- remote.write(s"${path.toString}${remote./}b", b)
        a <- remote.exists(path.toString)
        _ <- remote.remove(path.toString)
        b <- remote.exists(path.toString)
      } yield a && !b
      Runtime.default.unsafeRun(eff)
    },
    "deleteFile" -> Prop.forAll { path: JPath =>
      val eff = for {
        _ <- remote.mkdir(path.getParent.toString)
        _ <- remote.touch(path.toString)
        a <- remote.exists(path.toString)
        _ <- remote.remove(path.toString)
        b <- remote.exists(path.toString)
      } yield a && !b
      Runtime.default.unsafeRun(eff)
    }
  )

  def fileSysWithSymLinks = new DefaultRuleSet(
    "fileSysWithSymLinks",
    None,
    "linkFile" -> Prop.forAll { (src: JPath, dst: JPath, content: String) =>
      val eff = for {
        _ <- remote.mkdir(src.getParent.toString)
        _ <- remote.write(src.toString, content)
        _ <- remote.mkdir(dst.getParent.toString)
        _ <- remote.link(src.toString, dst.toString)
        c <- remote.read(dst.toString)
      } yield c == content
      Runtime.default.unsafeRun(eff)
    },
    "linkDir" -> Prop.forAll { (src: JPath, dst: JPath, a: String, b: String) =>
      val eff = for {
        _ <- remote.mkdir(src.toString)
        _ <- remote.write(s"${src.toString}${remote./}a", a)
        _ <- remote.write(s"${src.toString}${remote./}b", b)
        _ <- remote.mkdir(dst.toString)
        _ <- remote.link(src.toString, dst.toString)
        c <- remote.read(s"${dst.toString}${remote./}a")
        d <- remote.read(s"${dst.toString}${remote./}b")
      } yield (a == c) && (b == d)
      Runtime.default.unsafeRun(eff)
    }
  )

  def fileTransfer = new DefaultRuleSet(
    "fileTransfer",
    None,
//    "uploadThenDownloadFile" -> Prop.forAll { (p: JPath, q: JPath, r: JPath, content: String) =>
//      val eff = for {
//        _ <- local.mkdir(p.getParent.toString)
//        _ <- local.write(p.toString, content)
//        _ <- remote.mkdir(q.getParent.toString)
//        _ <- remote.upload(p.toString, q.toString)
//        a <- remote.read(q.toString)
//        _ <- local.mkdir(r.getParent.toString)
//        _ <- remote.download(q.toString, r.toString)
//        b <- local.read(r.toString)
//      } yield (a == content) && (b == content)
//      Runtime.default.unsafeRun(eff)
//    },
    "uploadThenDownloadDir" -> Prop.forAll { (p: JPath, q: JPath, r: JPath, a: String, b: String) =>
      val eff = for {
        _ <- local.mkdir(p.toString)
        _ <- local.write(s"${p.toString}${local./}a", a)
        _ <- local.write(s"${p.toString}${local./}b", b)
        _ <- remote.mkdir(q.getParent.toString)
        _ <- remote.upload(p.toString, q.toString)
        _ <- local.mkdir(r.getParent.toString)
        _ <- remote.download(q.toString, r.toString)
        c <- local.read(s"${r.toString}${local./}a")
        d <- local.read(s"${r.toString}${local./}b")
      } yield (a == c) && (b == d)
      Runtime.default.unsafeRun(eff)
    }
  )
}

class FileSysTest extends AnyFunSuite with FunSuiteDiscipline with Checkers with BeforeAndAfterAll {

  import FileSysGen._

  implicit val config: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 1, maxDiscardedFactor = 0.1)
  implicit val runtime: RuntimeConfig = RuntimeConfig.create(shell = "bash -eux", verbose = true)
  implicit val ctx: Context = new Context()

  val parser = new SemanticParser(ctx.root)
  parser.semanticParse(parser.readFileToStmts(File(getClass.getClassLoader.getResource("test-filesys.hm"))))
  val local = FileSys.local
  val s3 = FileSys("my_s3")
  val asb = FileSys("my_asb")
  val sftp = FileSys("my_sftp")
  val gcs = FileSys("my_gcs")
  implicit val stdSinks: StdSinks = StdSinks.default

//  override def beforeAll(): Unit = {
//    val eff = ctx.root.tasks("my_sftp.setup").default.script.executeLocally(runtime.workDir)
//    Runtime.default.unsafeRun(eff)
//  }

//  checkAll("local", new FileSysLaws(local, local).fileSys)
//  checkAll("local", new FileSysLaws(local, local).fileTransfer)
//  checkAll("aws.s3", new FileSysLaws(local, s3).fileSys)
//  checkAll("aws.s3", new FileSysLaws(local, s3).fileTransfer)
//  checkAll("gcloud.storage", new FileSysLaws(local, gcs).fileSys)
  checkAll("gcloud.storage", new FileSysLaws(local, gcs).fileTransfer)
//  checkAll("az.blob_storage", new FileSysLaws(local, asb).fileSys)
//  checkAll("az.blob_storage", new FileSysLaws(local, asb).fileTransfer)

}
