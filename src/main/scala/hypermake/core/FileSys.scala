package hypermake.core

import java.nio.file.{Files => JFiles, Paths}
import scala.collection._

import better.files._
import zio._
import zio.stream.ZSink
import zio.{Task => _, _}

import hypermake.exception.DataTransferFailedException
import hypermake.util._
import hypermake.util.logging._

/**
 * Encapsulates a file system that could be local, or some remote storage (e.g. SFTP, AWS S3, Azure Blob).
 * Such a file system must possess the capabilities of a basic file system.
 */
trait FileSys {

  /** Identifier of this file system. */
  def name: String

  /** Separator character used to separate path components. By default this is '/'. */
  def separator: Char

  /** Separator character used to separate a list of paths. By default this is ':'. */
  def pathSeparator: Char

  def / = separator

  def refreshInterval: Duration

  def maxFileNameLength: Int

  def supportsSymLinks: Boolean

  /** Reads the content of a file as a string. */
  def read(f: String)(implicit std: StdSinks): HIO[String]

  /** Writes a string to a given file. */
  def write(f: String, content: String)(implicit std: StdSinks): HIO[Unit]

  /**
   * Creates an empty directory at the given path.
   * This should have the semantics of `mkdir -p`: it should create all parent directories if they do not exist,
   * and it should not fail if the directory already exists.
   * If on a blob storage (e.g. AWS S3), this can always return true.
   */
  def mkdir(f: String)(implicit std: StdSinks): HIO[Unit]

  /** Checks if a file exists on this file system. */
  def exists(f: String)(implicit std: StdSinks): HIO[Boolean]

  /**
   * Creates a symbolic link from `src` to `dst`.
   * If symlink not supported, create a text file with the source path as content.
   * This behavior is consistent with git's behavior when encountering symlinks.
   */
  def link(src: String, dst: String)(implicit std: StdSinks): HIO[Unit]

  /** Creates an empty file at the given path. */
  def touch(f: String)(implicit std: StdSinks): HIO[Unit]

  /** Removes a file or directory. If a directory, recursively remove all its contents. */
  def remove(f: String)(implicit std: StdSinks): HIO[Unit]

  /** Uploads a file or directory from the local file system to this file system. */
  def upload(src: String, dst: String)(implicit std: StdSinks): HIO[Unit]

  /** Downloads a file or directory from this file system to the local file system. */
  def download(src: String, dst: String)(implicit std: StdSinks): HIO[Unit]

  /** Computes a checksum of a file. Returns the hex-encoded hash string. */
  def checksum(file: String)(implicit std: StdSinks): HIO[String]

  def copyFrom(src: String, srcFs: FileSys, dst: String)(implicit ctx: Context, std: StdSinks): HIO[Unit] = {
    if (srcFs == this)
      link(src, dst)
    else if (srcFs == ctx.local)
      upload(src, dst)
    else if (this == ctx.local)
      srcFs.download(src, dst)
    else FileSys.copy(src, srcFs, dst, this)
  }

  def isLocked(f: String)(implicit std: StdSinks): HIO[Boolean] = exists(s"$f${/}.lock")

  def lock(f: String)(implicit std: StdSinks): HIO[Unit] =
    isLocked(f).delay(refreshInterval).repeatUntilEquals(false) *> touch(s"$f${/}.lock")

  def unlock(f: String)(implicit std: StdSinks): HIO[Unit] =
    isLocked(f).delay(refreshInterval).repeatUntilEquals(true) *> remove(s"$f${/}.lock")

  def forceUnlock(f: String)(implicit std: StdSinks): HIO[Unit] = for {
    isLocked <- isLocked(f)
    u <- if (isLocked) remove(s"$f${/}.lock") else ZIO.unit
  } yield u

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

  def getScriptByNameOpt(name: String)(implicit ctx: Context) =
    ctx.root.functions.get(name).map(_.impl.default)

  def getTaskByName(name: String)(implicit ctx: Context) = ctx.root.tasks(name).default

  def getTaskByNameOpt(str: String)(implicit ctx: Context) = ctx.root.tasks.get(str).map(_.default)

  def getScriptByNamesOpt(names: String*)(implicit ctx: Context) =
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
    lazy val maxFileNameLength =
      ctx.root.values.get("local.max_file_name_length").map(_.default.value.toInt).getOrElse(255)
    val refreshInterval = 100.milliseconds
    val supportsSymLinks = true

    def read(f: String)(implicit std: StdSinks) = ZIO.attempt {
      File(resolvePath(f)).contentAsString
    }

    def write(f: String, content: String)(implicit std: StdSinks) = ZIO.attempt {
      File(resolvePath(f)).writeText(content)
    }

    def mkdir(f: String)(implicit std: StdSinks) = ZIO.attempt {
      File(resolvePath(f)).createDirectoryIfNotExists(createParents = true)
    }

    def exists(f: String)(implicit std: StdSinks) = ZIO.attempt {
      File(resolvePath(f)).exists
    }

    def touch(f: String)(implicit std: StdSinks) = ZIO.attempt {
      File(resolvePath(f)).touch()
    }

    def remove(f: String)(implicit std: StdSinks) = ZIO.attempt {
      File(resolvePath(f)).delete(swallowIOExceptions = true)
    }

    def link(src: String, dst: String)(implicit std: StdSinks) = ZIO.attempt {
      val dstPath = Paths.get(resolvePath(dst))
      val relativePath = dstPath.getParent.relativize(Paths.get(resolvePath(src)))
      JFiles.deleteIfExists(dstPath)
      JFiles.createSymbolicLink(dstPath, relativePath)
    }

    def upload(src: String, dst: String)(implicit std: StdSinks): HIO[Unit] = link(src, dst)

    def download(src: String, dst: String)(implicit std: StdSinks): HIO[Unit] = link(src, dst)

    def checksum(f: String)(implicit std: StdSinks): HIO[String] = ZIO.attempt {
      val path = File(resolvePath(f))
      val md = java.security.MessageDigest.getInstance("SHA-256")

      def hashFile(file: File): Unit = {
        val buffer = new Array[Byte](8192)
        val fis = file.newInputStream
        try {
          var bytesRead = fis.read(buffer)
          while (bytesRead != -1) {
            md.update(buffer, 0, bytesRead)
            bytesRead = fis.read(buffer)
          }
        } finally {
          fis.close()
        }
      }

      if (path.isDirectory) {
        // For directories: hash all files in sorted order, including relative paths for determinism
        val files = path.listRecursively.filter(_.isRegularFile).toSeq.sortBy(_.path.toString)
        for (file <- files) {
          val relativePath = path.relativize(file).toString
          md.update(relativePath.getBytes("UTF-8"))
          hashFile(file)
        }
      } else {
        hashFile(path)
      }

      md.digest().map("%02x".format(_)).mkString
    }

  }

  /** A custom file system from an object definition. */
  class Custom(val name: String)(implicit ctx: Context) extends FileSys {

    import ctx._

    lazy val separator =
      getValueByNameOpt(s"${name}.separator").map(_.head).getOrElse(java.io.File.separatorChar)

    lazy val pathSeparator =
      getValueByNameOpt(s"${name}.path_separator")
        .map(_.head)
        .getOrElse(java.io.File.pathSeparatorChar)

    lazy val maxFileNameLength =
      getValueByNameOpt(s"${name}.max_file_name_length").map(_.toInt).getOrElse(255)

    lazy val root = getValueByName(s"${name}.root")

    lazy val refreshInterval =
      getValueByNameOpt(s"${name}.refresh_interval")
        .map(_.toInt)
        .getOrElse(5)
        .seconds // by default, 5s

    lazy val supportsSymLinks =
      getScriptByNameOpt(s"${name}.link").isDefined

    def read(f: String)(implicit std: StdSinks) = {
      val baos = new java.io.ByteArrayOutputStream()
      val sinks = std.copy(
        out = ZSink.fromOutputStream(baos),
        err = std.err
      )
      for {
        _ <- logCall(s"$name.read", f)
        exitCode <- local.executeScript(
          getScriptByName(s"${name}.read")
            .withArgs("file" -> f),
          wd = ctx.runtime.workDir
        )
        _ <- logExitCode(s"$name.read", f)(exitCode)
      } yield baos.toString()
    }

    def write(f: String, content: String)(implicit std: StdSinks) = {
      val tempScriptFile = runtime.newTempFile()
      for {
        _ <- logCall(s"$name.write", f)
        _ <- ZIO.attempt {
          File(tempScriptFile).writeText(content)
        }
        u <- upload(tempScriptFile, f)
      } yield u
    }

    def mkdir(f: String)(implicit std: StdSinks) = for {
      _ <- logCall(s"$name.mkdir", f)
      exitCode <- local.executeScript(
        getScriptByName(s"$name.mkdir")
        .withArgs("dir" -> f),
        wd = ctx.runtime.workDir
      )
      _ <- logExitCode(s"$name.mkdir", f)(exitCode)
      u <- exitCode.successfulExitCode.unit
    } yield u

    def exists(f: String)(implicit std: StdSinks) = for {
      _ <- logCall(s"$name.exists", f)
      process <- getScriptByName(s"$name.exists")
        .withArgs("file" -> f)
        .executeLocally(ctx.runtime.workDir)
      exitCode <- process.exitCode
      _ <- logExitCode(s"$name.exists", f)(process)
    } yield exitCode.code == 0

    def link(src: String, dst: String)(implicit std: StdSinks) = {
      if (supportsSymLinks) {
        for {
          _ <- logCall(s"$name.link", src, dst)
          process <- getScriptByName(s"${name}.link")
            .withArgs("src" -> src, "dst" -> dst)
            .executeLocally(ctx.runtime.workDir)
          _ <- logExitCode(s"$name.link", src, dst)(process)
          u <- process.successfulExitCode.unit
        } yield u
      } else {
        // When symlink is not supported, mimics git's behavior when encountering symlinks:
        // Create a text file with the source path as content.
        for {
          u <- ctx.local.write(dst, src)
        } yield u
      }
    }

    def touch(f: String)(implicit std: StdSinks) = for {
      _ <- logCall(s"$name.touch", f)
      process <- getScriptByName(s"$name.touch")
        .withArgs("file" -> f)
        .executeLocally(ctx.runtime.workDir)
      _ <- logExitCode(s"$name.touch", f)(process)
      u <- process.successfulExitCode.unit
    } yield u

    def remove(f: String)(implicit std: StdSinks) = for {
      _ <- logCall(s"$name.remove", f)
      process <- getScriptByName(s"$name.remove")
        .withArgs("file" -> f)
        .executeLocally(ctx.runtime.workDir)
      _ <- logExitCode(s"$name.remove", f)(process)
      u <- process.successfulExitCode.unit
    } yield u

    def upload(src: String, dst: String)(implicit std: StdSinks): HIO[Unit] = for {
      _ <- logCall(s"$name.upload", src, dst)
      process <- ctx.root
        .functions(s"${name}.upload")
        .partial(Args.from("src" -> src, "dst" -> dst))
        .reify
        .default
        .executeLocally(ctx.local.root)
      _ <- logExitCode(s"$name.upload", src, dst)(process)
      u <- process.successfulExitCode.unit
    } yield u

    def download(src: String, dst: String)(implicit std: StdSinks): HIO[Unit] = for {
      _ <- logCall(s"$name.download", src, dst)
      process <- ctx.root
        .functions(s"${name}.download")
        .partial(Args.from("src" -> src, "dst" -> dst))
        .reify
        .default
        .executeLocally(ctx.local.root)
      _ <- logExitCode(s"$name.download", src, dst)(process)
      u <- process.successfulExitCode.unit
    } yield u

    def checksum(file: String)(implicit std: StdSinks): HIO[String] = {
      val baos = new java.io.ByteArrayOutputStream()
      val sinks = StdSinks(ZSink.fromOutputStream(baos), std.err)
      for {
        _ <- logCall(s"$name.checksum", file)
        process <- ctx.root.functions(s"${name}.checksum")
          .partial(Args.from("file" -> file))
          .reify
          .default
          .executeLocally(ctx.local.root)(ctx.runtime, sinks)
        _ <- logExitCode(s"$name.checksum", file)(process)
      } yield baos.toString().trim
    }

    def asService: Option[Service] = for {
      start <- getTaskByNameOpt(s"${name}.start")
      stop <- getTaskByNameOpt(s"${name}.stop")
    } yield Service(start, stop)

  }

  /** Copy between file systems. */
  def copy(src: String, srcFs: FileSys, dst: String, dstFs: FileSys)(implicit
      ctx: Context,
      std: StdSinks
  ): HIO[Unit] = for {
    _ <- logCall(s"copy_from_${srcFs}_to_${dstFs}", src, dst)(ctx.runtime)
    process <- getScriptByName(s"copy_from_${srcFs}_to_${dstFs}")
      .withArgs("src" -> src, "dst" -> dst)
      .executeLocally(ctx.runtime.workDir)(ctx.runtime, std)
    exitCode <- process.exitCode
    u <- ZIO.fail(DataTransferFailedException(srcFs.name, src)).unless(exitCode.code == 0)
  } yield u

}
