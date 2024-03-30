package hypermake.core

import java.nio.file.{Files => JFiles, Paths}
import scala.collection._

import better.files._
import zio._
import zio.duration._
import zio.process._

import hypermake.exception.DataTransferFailedException
import hypermake.semantics.Context
import hypermake.util.Escaper.Shell
import hypermake.util._

/**
 * Encapsulates a file system that could be local, or some remote grid (e.g. SFTP, or AWS S3).
 * Such a file system must possess the capabilities of a basic file system.
 * Optionally it can be equipped with methods to run arbitrary shell scripts.
 */
trait FileSys {

  /** Identifier of this file system. */
  def name: String

  /** Separator character used to separate path components. By default this is '/'. */
  def separator: Char

  /** Separator character used to separate a list of paths. By default this is ':'. */
  def pathSeparator: Char

  def / = separator

  /** Output root on this file system. Intermediate results will be stored in this directory. */
  def root: String

  def refreshInterval: Duration

  /** Resolves a path relative to the given root directory. */
  def resolvePath(s: String, r: String = root) = {
    val p = s.trim
    if (p startsWith separator.toString) p
    else r + separator + p
  }

  /** Reads the content of a file as a string. */
  def read(f: String)(implicit std: StdSinks): HIO[String]

  /** Writes a string to a given file. */
  def write(f: String, content: String)(implicit std: StdSinks): HIO[Unit]

  /**
   * Creates an empty directory at the given path.
   *  If on a blob storage (e.g. AWS S3), this can always return true.
   */
  def mkdir(f: String)(implicit std: StdSinks): HIO[Unit]

  /** Checks if a file exists on this file system. */
  def exists(f: String)(implicit std: StdSinks): HIO[Boolean]

  /** Creates a symbolic link from `src` to `dst`. */
  def link(src: String, dst: String)(implicit std: StdSinks): HIO[Unit]

  /** Creates an empty file at the given path. */
  def touch(f: String)(implicit std: StdSinks): HIO[Unit]

  def delete(f: String)(implicit std: StdSinks): HIO[Unit]

  def upload(src: String, dst: String)(implicit std: StdSinks): HIO[Unit]

  def download(src: String, dst: String)(implicit std: StdSinks): HIO[Unit]

  def copyFrom(src: String, srcFs: FileSys, dst: String)(implicit ctx: Context, std: StdSinks): HIO[Unit] = {
    if (srcFs == this)
      link(src, dst)
    else if (srcFs == ctx.local)
      upload(src, dst)
    else if (this == ctx.local)
      srcFs.download(src, dst)
    else FileSys.copy(src, srcFs, dst, this)
  }

  def execute(wd: String, command: String, args: Seq[String], envArgs: Map[String, String])(implicit
      std: StdSinks
  ): HIO[ExitCode]

  def isLocked(f: String)(implicit std: StdSinks): HIO[Boolean] = exists(s"$f${/}.lock")

  def lock(f: String)(implicit std: StdSinks): HIO[Unit] =
    isLocked(f).delay(refreshInterval).repeatUntilEquals(false) *> touch(s"$f${/}.lock")

  def unlock(f: String)(implicit std: StdSinks): HIO[Unit] =
    isLocked(f).delay(refreshInterval).repeatUntilEquals(true) *> delete(s"$f${/}.lock")

  def forceUnlock(f: String)(implicit std: StdSinks): HIO[Unit] = for {
    isLocked <- isLocked(f)
    u <- if (isLocked) delete(s"$f${/}.lock") else ZIO.succeed(())
  } yield u

  def linkValue(x: Value, dst: String)(implicit ctx: Context, std: StdSinks): HIO[Option[String]] = {
    x match {
      case Value.Pure(_) => ZIO.none // do nothing
      case Value.Input(path, fs) =>
        val e =
          if (fs == this) link(path, dst)
          else copyFrom(path, fs, dst)
        e as Some(dst)
      case Value.PackageOutput(pack) =>
        val p = pack.output.on(this).value
        link(p, dst) as Some(dst)
      case Value.Output(path, fs, job) =>
        val e =
          if (fs == this) link(resolvePath(path, job.absolutePath), dst)
          else copyFrom(fs.resolvePath(path, job.absolutePath), fs, dst)
        e as Some(dst)
      case Value.Multiple(values, _) =>
        for {
          _ <- mkdir(dst)
          r <- ZIO.foreachPar(values.allPairs) { case (c, v) =>
            val argsString = ctx.percentEncodedCaseStringPath(c)
            linkValue(v, s"$dst${/}$argsString") as s"$dst${/}$argsString"
          }
        } yield Some(r.mkString(" "))
    }
  }

  override def toString = name

  override def equals(o: Any) = o match {
    case o: FileSys => name == o.name
    case _          => false
  }

}

object FileSys {

  def getValueByName(name: String)(implicit ctx: Context) = ctx.root.values(name).default.value

  def getValueByNameOpt(name: String)(implicit ctx: Context) =
    ctx.root.values.get(name).map(_.default.value)

  def getScriptByName(name: String)(implicit ctx: Context) = ctx.root.functions(name).impl.default

  def getScriptByNames(names: String*)(implicit ctx: Context) =
    names.foldLeft[Option[Script]](None) { (acc, name) =>
      acc.orElse(ctx.root.functions.get(name).map(_.impl.default))
    }

  def apply(name: String)(implicit ctx: Context) = {
    if (name == "local")
      ctx.local
    else if (ctx.fsTable contains name)
      ctx.getFs(name)
    else {
      val fs = new FileSys.Custom(name)
      ctx.fsTable += name -> fs
      fs
    }
  }

  def local(implicit ctx: Context) = ctx.local

  /** The local file system (where HyperMake runs). */
  class Local(implicit ctx: Context) extends FileSys {

    final val name = "local"
    val separator = java.io.File.separatorChar
    val pathSeparator = java.io.File.pathSeparatorChar
    lazy val root = ctx.root.values.get("local.root").map(_.default.value).getOrElse("out")

    def refreshInterval = 100.milliseconds

    def read(f: String)(implicit std: StdSinks) = IO {
      File(f).contentAsString
    }

    def write(f: String, content: String)(implicit std: StdSinks) = IO {
      File(f).writeText(content)
    }

    def mkdir(f: String)(implicit std: StdSinks) = IO {
      File(f).createDirectoryIfNotExists(createParents = true)
    }

    def exists(f: String)(implicit std: StdSinks) = IO {
      File(f).exists
    }

    def touch(f: String)(implicit std: StdSinks) = IO {
      File(f).touch()
    }

    def delete(f: String)(implicit std: StdSinks) = IO {
      File(f).delete(swallowIOExceptions = true)
    }

    def link(src: String, dst: String)(implicit std: StdSinks) = IO {
      val dstPath = Paths.get(dst)
      val relativePath = dstPath.getParent.relativize(Paths.get(src))
      JFiles.deleteIfExists(dstPath)
      JFiles.createSymbolicLink(dstPath, relativePath)
    }

    def upload(src: String, dst: String)(implicit std: StdSinks): HIO[Unit] = link(src, dst)

    def download(src: String, dst: String)(implicit std: StdSinks): HIO[Unit] = link(src, dst)

    override def execute(
        wd: String,
        command: String,
        args: Seq[String],
        envArgs: Map[String, String]
    )(implicit
        std: StdSinks
    ) = {
      val interpreter :: interpreterArgs = command.split(' ').toList
      val resolvedWd = resolvePath(wd)
      val pb = zio.process
        .Command(interpreter, (interpreterArgs ++ args): _*)
        .workingDirectory(File(resolvedWd).toJava.getAbsoluteFile)
        .env(envArgs.toMap)
        .stderr(ProcessOutput.Pipe)
        .stdout(ProcessOutput.Pipe)
      for {
        process <- pb.run
        _ <- process.stdout.stream.run(std.out) <&> process.stderr.stream.run(std.err)
        exitCode <- process.exitCode
        _ <- write(s"$resolvedWd${/}exitcode", exitCode.code.toString)
      } yield exitCode
    }

  }

  /** A custom file system from an object definition. */
  class Custom(val name: String)(implicit ctx: Context) extends FileSys {

    import ctx._

    def separator =
      getValueByNameOpt(s"${name}.separator").map(_.head).getOrElse(java.io.File.separatorChar)

    def pathSeparator =
      getValueByNameOpt(s"${name}.path_separator")
        .map(_.head)
        .getOrElse(java.io.File.pathSeparatorChar)

    def root = getValueByName(s"${name}.root")

    def refreshInterval =
      getValueByNameOpt(s"${name}.refresh_interval")
        .map(_.toInt)
        .getOrElse(5)
        .seconds // by default, 5s

    def read(f: String)(implicit std: StdSinks) = for {
      process <- getScriptByName(s"${name}.read")
        .withArgs("file" -> f)
        .executeLocally(ctx.runtime.workDir)
      stdout <- process.stdout.string
    } yield stdout

    def write(f: String, content: String)(implicit std: StdSinks) = {
      val tempScriptFile = runtime.newTempFile()
      for {
        _ <- IO {
          File(tempScriptFile).write(content)
        }
        u <- upload(tempScriptFile, f)
      } yield u
    }

    def mkdir(f: String)(implicit std: StdSinks) = for {
      process <- getScriptByName(s"$name.mkdir")
        .withArgs("dir" -> f)
        .executeLocally(ctx.runtime.workDir)
      u <- process.successfulExitCode.unit
    } yield u

    def exists(f: String)(implicit std: StdSinks) = for {
      process <- getScriptByName(s"$name.exists")
        .withArgs("file" -> f)
        .executeLocally(ctx.runtime.workDir)
      exitCode <- process.exitCode
    } yield exitCode.code == 0

    def link(src: String, dst: String)(implicit std: StdSinks) = for {
      process <- getScriptByName(s"${name}.link")
        .withArgs("src" -> src, "dst" -> dst)
        .executeLocally(ctx.runtime.workDir)
      u <- process.successfulExitCode.unit
    } yield u

    def touch(f: String)(implicit std: StdSinks) = for {
      process <- getScriptByName(s"$name.touch")
        .withArgs("file" -> f)
        .executeLocally(ctx.runtime.workDir)
      u <- process.successfulExitCode.unit
    } yield u

    def delete(f: String)(implicit std: StdSinks) = for {
      process <- getScriptByName(s"$name.delete")
        .withArgs("file" -> f)
        .executeLocally(ctx.runtime.workDir)
      u <- process.successfulExitCode.unit
    } yield u

    def upload(src: String, dst: String)(implicit std: StdSinks): HIO[Unit] = for {
      process <- getScriptByName(s"${name}.upload")
        .withArgs("src" -> src, "dst" -> dst)
        .executeLocally(ctx.runtime.workDir)
      u <- process.successfulExitCode.unit
    } yield u

    def download(src: String, dst: String)(implicit std: StdSinks): HIO[Unit] = for {
      process <- getScriptByName(s"${name}.download")
        .withArgs("src" -> src, "dst" -> dst)
        .executeLocally(ctx.runtime.workDir)
      u <- process.successfulExitCode.unit
    } yield u

    def execute(wd: String, command: String, args: Seq[String], envVars: Map[String, String])(implicit std: StdSinks) = for {
      process <- getScriptByName(s"${name}.execute")
        .withArgs(
          "command" ->
            s"""cd ${resolvePath(wd)};
               | ${envVars.map { case (k, v) => s"$k=${Shell.escape(v)}" }.mkString(" ")}
               | $command ${args.mkString(" ")} > stdout 2> stderr
               |""".stripMargin.replace("\n", "")
        )
        .executeLocally(FileSys.local.resolvePath(wd))
      _ <- process.stdout.stream.run(std.out) <&> process.stderr.stream.run(std.err)
      exitCode <- process.exitCode
    } yield exitCode

  }

  /** Copy between environments. */
  def copy(src: String, srcFs: FileSys, dst: String, dstFs: FileSys)(implicit
      ctx: Context,
      std: StdSinks
  ): HIO[Unit] = for {
    process <- getScriptByName(s"copy_from_${srcFs}_to_${dstFs}")
      .withArgs("src" -> src, "dst" -> dst)
      .executeLocally(ctx.runtime.workDir)(ctx.runtime, std)
    exitCode <- process.exitCode
    u <-
      if (exitCode.code == 0) ZIO.succeed(())
      else ZIO.fail(DataTransferFailedException(srcFs.name, src))
  } yield u

}
