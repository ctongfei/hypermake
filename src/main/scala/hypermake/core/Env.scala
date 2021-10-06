package hypermake.core

import java.io.{File => JFile}
import java.nio.file.{Path, Paths, Files => JFiles}
import scala.collection._
import better.files.File
import zio._
import zio.process._
import zio.stream._
import zio.duration._
import hypermake.cli._
import hypermake.collection._
import hypermake.semantics.Context
import hypermake.util._

/**
 * Encapsulates a running environment that could be local, or some remote grid.
 * Such an environment must possess a basic file system, as well as the capability
 * to run arbitrary shell script.
 */
trait Env {

  /**
   * Identifier of this environment.
   */
  def name: String

  /**
   * Separator character used to separate path components. By default this is '/'.
   */
  def separator: Char

  /**
   * Separator character used to separate a list of paths. By default this is ':'.
   */
  def pathSeparator: Char

  /**
   * Output root on this environment. Intermediate results will be stored in this directory.
   */
  def root: String

  def refreshInterval: Duration

  /**
   * Resolves a path relative to the given root directory.
   */
  def resolvePath(s: String, r: String = root) = {
    val p = s.trim
    if (p startsWith separator.toString) p
    else r + separator + p
  }

  /**
   * Reads the content of a file as a string.
   */
  def read(f: String): HIO[String]

  def write(f: String, content: String): HIO[Unit]

  def mkdir(f: String): HIO[Unit]

  /**
   * Checks if a file exists on this file system.
   */
  def exists(f: String): HIO[Boolean]

  /**
   * Creates a symbolic link from [[src]] to [[dst]].
   */
  def link(src: String, dst: String): HIO[Unit]

  /**
   * Creates an empty file at the given path.
   */
  def touch(f: String): HIO[Unit]

  def delete(f: String): HIO[Unit]
  def copyFrom(src: String, srcEnv: Env, dst: String): HIO[Unit]
  def execute(wd: String, command: String, args: Seq[String], envArgs: Map[String, String], out: HSink[Byte], err: HSink[Byte]): HIO[ExitCode]

  def isLocked(f: String): HIO[Boolean] = exists(s"$f/.lock")
  def lock(f: String): HIO[Unit] = isLocked(f).delay(refreshInterval).repeatUntilEquals(false) *> touch(s"$f/.lock")
  def unlock(f: String): HIO[Unit] = isLocked(f).delay(refreshInterval).repeatUntilEquals(true) *> delete(s"$f/.lock")
  def forceUnlock(f: String): HIO[Unit] = for {
    isLocked <- isLocked(f)
    u <- if (isLocked) delete(s"$f/.lock") else ZIO.succeed(())
  } yield u

  def linkValue(x: Value, dst: String)(implicit ctx: Context): HIO[Option[String]] = {
    x match {
      case Value.Pure(_) => ZIO.none  // do nothing
      case Value.Input(path, env) =>
        val e = if (env == this) link(path, dst)
        else copyFrom(path, env, dst)
        e as Some(dst)
      case Value.PackageOutput(pack) =>
        val p = pack.output.on(this).value
        link(p, dst) as Some(dst)
      case Value.Output(path, env, job) =>
        val e = if (env == this) link(resolvePath(path, job.absolutePath), dst)
        else copyFrom(env.resolvePath(path, job.absolutePath), env, dst)
        e as Some(dst)
      case Value.Multiple(values, _) =>
        for {
          _ <- mkdir(dst)
          r <- ZIO.foreachPar(values.allPairs) { case (c, v) =>
            val argsString = ctx.escapedArgsString(c.underlying)
            linkValue(v, s"$dst/$argsString") as s"$dst/$argsString"
          }
        } yield Some(r.mkString(ctx.runtime.IFS_CHAR))
    }
  }

  override def toString = name

  override def equals(o: Any) = o match {
    case o: Env => name == o.name
    case _ => false
  }

}

object Env {

  def getValueByName(name: String)(implicit ctx: Context) = ctx.getValue(Name(name)).default.value
  def getValueByNameOpt(name: String)(implicit ctx: Context) = ctx.getValueOpt(Name(name)).map(_.default.value)

  def getScriptByName(name: String)(implicit ctx: Context) = ctx.getFunc(Name(name)).impl.default

  def apply(name: Name)(implicit ctx: Context) = {
    if (name.name == "local")
      ctx.localEnv
    else if (ctx.envTable contains name)
      ctx.getEnv(name)
    else {
      val env = new Env.Custom(name.name)
      ctx.envTable += name -> env
      env
    }
  }

  def local(implicit ctx: Context) = ctx.localEnv

  class Local(implicit ctx: Context) extends Env {

    import ctx._

    final def name = "local"
    def separator = java.io.File.separatorChar
    def pathSeparator = java.io.File.pathSeparatorChar
    def root = ctx.envOutputRoot(Name("local"))
    def refreshInterval = 100.milliseconds

    def read(f: String) = IO {
      File(f).contentAsString
    }

    def write(f: String, content: String) = IO {
      File(f).writeText(content)
    }

    def mkdir(f: String) = IO {
      File(f).createDirectoryIfNotExists(createParents = true)
    }

    def exists(f: String) = IO {
      File(f).exists
    }

    def touch(f: String) = IO {
      File(f).touch()
    }

    def delete(f: String) = IO {
      File(f).delete(swallowIOExceptions = true)
    }

    def link(src: String, dst: String) = IO {
      val dstPath = Paths.get(dst)
      val relativePath = dstPath.getParent.relativize(Paths.get(src))
      JFiles.deleteIfExists(dstPath);
      JFiles.createSymbolicLink(dstPath, relativePath)
    }

    override def copyFrom(src: String, srcEnv: Env, dst: String) = for {
      process: Process <- getScriptByName(s"copy_from_${srcEnv}_to_local")
        .withArgs("src" -> src, "dst" -> dst)
        .executeLocally()
      u <- process.successfulExitCode.unit
    } yield u

    override def execute(wd: String, command: String, args: Seq[String], envArgs: Map[String, String], out: HSink[Byte], err: HSink[Byte]) = {
      val interpreter :: interpreterArgs = command.split(' ').toList
      val proc = zio.process.Command(interpreter, (interpreterArgs ++ args): _ *)
        .workingDirectory(new JFile(wd)).env(envArgs.toMap)
        .stderr(ProcessOutput.Pipe).stdout(ProcessOutput.Pipe)
      //ZSink.fromFile(Paths.get(s"$wd/stdout"))
      for {
        process <- proc.run
        _ <- process.stdout.stream.run(out)
        _ <- process.stderr.stream.run(err)
        exitCode <- process.exitCode
        _ <- write(s"$wd/exitcode", exitCode.code.toString)
      } yield exitCode
    }

  }

  class Custom(val name: String)(implicit ctx: Context) extends Env {

    import ctx._

    def separator =
      getValueByNameOpt(s"${name}_separator").map(_.head).getOrElse(java.io.File.separatorChar)

    def pathSeparator =
      getValueByNameOpt(s"${name}_path_separator").map(_.head).getOrElse(java.io.File.pathSeparatorChar)

    def root = getValueByName(s"${name}_root")

    def refreshInterval =
      getValueByNameOpt(s"${name}_refresh_interval").map(_.toInt).getOrElse(5).seconds  // by default, 5s

    def read(f: String) = for {
      process <- getScriptByName(s"${name}_read").withArgs("file" -> f).executeLocally()
      stdout <- process.stdout.string
    } yield stdout

    def write(f: String, content: String) = {
      val tempScriptFile = runtime.tempFile()
      for {
        _ <- IO {
          File(tempScriptFile).write(content)
        }
        u <- copyFrom(tempScriptFile, ctx.localEnv, f)
      } yield u
    }

    def mkdir(f: String) = for {
      process <- getScriptByName(s"${name}_mkdir").withArgs("dir" -> f).executeLocally()
      u <- process.successfulExitCode.unit
    } yield u

    def exists(f: String) = for {
      process <- getScriptByName(s"${name}_exists").withArgs("file" -> f).executeLocally()
      exitCode <- process.exitCode
    } yield exitCode.code == 0

    def link(src: String, dst: String) = for {
      process <- getScriptByName(s"${name}_link").withArgs("src" -> src, "dst" -> dst).executeLocally()
      u <- process.successfulExitCode.unit
    } yield u

    def touch(f: String) = for {
      process <- getScriptByName(s"${name}_touch").withArgs("file" -> f).executeLocally()
      u <- process.successfulExitCode.unit
    } yield u

    def delete(f: String) = for {
      process <- getScriptByName(s"${name}_delete").withArgs("file" -> f).executeLocally()
      u <- process.successfulExitCode.unit
    } yield u

    def copyFrom(src: String, srcEnv: Env, dst: String) = for {
      process <- getScriptByName(s"copy_from_${srcEnv}_to_$name")
        .withArgs("src" -> src, "dst" -> dst).executeLocally()
      u <- process.successfulExitCode.unit
    } yield u

    def execute(wd: String, command: String, args: Seq[String], envVars: Map[String, String], out: HSink[Byte], err: HSink[Byte]) = for {
      process <- getScriptByName(s"${name}_execute")
        .withArgs("command" ->
          s"${envVars.map { case (k, v) => s"$k=$v" }.mkString(" ")} $command ${args.mkString(" ")}"
        ).executeLocally(wd)
      _ <- process.stdout.stream.run(out)
      _ <- process.stderr.stream.run(err)
      exitCode <- process.exitCode
    } yield exitCode

  }
}
