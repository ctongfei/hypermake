package hypermake.core

import better.files.File
import hypermake.collection._
import hypermake.exception.DataTransferFailedException
import hypermake.semantics.Context
import hypermake.util._
import zio._
import zio.duration._
import zio.process._

import java.nio.file.{Paths, Files => JFiles}
import scala.collection._

/** Encapsulates a running environment that could be local, or some remote grid. Such an environment must possess a
 * basic file system, as well as the capability to run arbitrary shell script.
 */
trait Env {

  /** Identifier of this environment.
   */
  def name: String

  /** Separator character used to separate path components. By default this is '/'.
   */
  def separator: Char

  /** Separator character used to separate a list of paths. By default this is ':'.
   */
  def pathSeparator: Char

  def / = separator

  /** Output root on this environment. Intermediate results will be stored in this directory.
   */
  def root: String

  def refreshInterval: Duration

  /** Resolves a path relative to the given root directory.
   */
  def resolvePath(s: String, r: String = root) = {
    val p = s.trim
    if (p startsWith separator.toString) p
    else r + separator + p
  }

  /** Reads the content of a file as a string.
   */
  def read(f: String)(implicit std: StdSinks): HIO[String]

  def write(f: String, content: String)(implicit std: StdSinks): HIO[Unit]

  def mkdir(f: String)(implicit std: StdSinks): HIO[Unit]

  /** Checks if a file exists on this file system.
   */
  def exists(f: String)(implicit std: StdSinks): HIO[Boolean]

  /** Creates a symbolic link from [[src]] to [[dst]].
   */
  def link(src: String, dst: String)(implicit std: StdSinks): HIO[Unit]

  /** Creates an empty file at the given path.
   */
  def touch(f: String)(implicit std: StdSinks): HIO[Unit]

  def delete(f: String)(implicit std: StdSinks): HIO[Unit]

  def copyFrom(src: String, srcEnv: Env, dst: String)(implicit std: StdSinks): HIO[Unit]

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
      case Value.Input(path, env) =>
        val e =
          if (env == this) link(path, dst)
          else copyFrom(path, env, dst)
        e as Some(dst)
      case Value.PackageOutput(pack) =>
        val p = pack.output.on(this).value
        link(p, dst) as Some(dst)
      case Value.Output(path, env, job) =>
        val e =
          if (env == this) link(resolvePath(path, job.absolutePath), dst)
          else copyFrom(env.resolvePath(path, job.absolutePath), env, dst)
        e as Some(dst)
      case Value.Multiple(values, _) =>
        for {
          _ <- mkdir(dst)
          r <- ZIO.foreachPar(values.allPairs) { case (c, v) =>
            val argsString = ctx.percentEncodedCaseString(c)
            linkValue(v, s"$dst${/}$argsString") as s"$dst${/}$argsString"
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

    final val name = "local"
    val separator = java.io.File.separatorChar
    val pathSeparator = java.io.File.pathSeparatorChar
    lazy val root = ctx.envOutputRoot(Name("local"))

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
      JFiles.deleteIfExists(dstPath);
      JFiles.createSymbolicLink(dstPath, relativePath)
    }

    override def copyFrom(src: String, srcEnv: Env, dst: String)(implicit std: StdSinks) = for {
      proc <- getScriptByName(s"copy_from_${srcEnv}_to_local")
        .withArgs("src" -> src, "dst" -> dst)
        .executeLocally()
      exitCode <- proc.exitCode
      u <- if (exitCode.code == 0) ZIO.succeed(()) else ZIO.fail(DataTransferFailedException(srcEnv.name, src))
    } yield u

    override def execute(wd: String, command: String, args: Seq[String], envArgs: Map[String, String])(implicit
                                                                                                       std: StdSinks
    ) = {
      val interpreter :: interpreterArgs = command.split(' ').toList
      val pb = zio.process
        .Command(interpreter, (interpreterArgs ++ args): _*)
        .workingDirectory(File(wd).toJava.getAbsoluteFile)
        .env(envArgs.toMap)
        .stderr(ProcessOutput.Pipe)
        .stdout(ProcessOutput.Pipe)
      for {
        process <- pb.run
        _ <- process.stdout.stream.run(std.out) <&> process.stderr.stream.run(std.err)
        exitCode <- process.exitCode
        _ <- write(s"$wd${/}exitcode", exitCode.code.toString)
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
      getValueByNameOpt(s"${name}_refresh_interval").map(_.toInt).getOrElse(5).seconds // by default, 5s

    def read(f: String)(implicit std: StdSinks) = for {
      process <- getScriptByName(s"${name}_read").withArgs("file" -> f).executeLocally()
      stdout <- process.stdout.string
    } yield stdout

    def write(f: String, content: String)(implicit std: StdSinks) = {
      val tempScriptFile = runtime.tempFile()
      for {
        _ <- IO {
          File(tempScriptFile).write(content)
        }
        u <- copyFrom(tempScriptFile, ctx.localEnv, f)
      } yield u
    }

    def mkdir(f: String)(implicit std: StdSinks) = for {
      process <- getScriptByName(s"${name}_mkdir").withArgs("dir" -> f).executeLocally()
      u <- process.successfulExitCode.unit
    } yield u

    def exists(f: String)(implicit std: StdSinks) = for {
      process <- getScriptByName(s"${name}_exists").withArgs("file" -> f).executeLocally()
      exitCode <- process.exitCode
    } yield exitCode.code == 0

    def link(src: String, dst: String)(implicit std: StdSinks) = for {
      process <- getScriptByName(s"${name}_link").withArgs("src" -> src, "dst" -> dst).executeLocally()
      u <- process.successfulExitCode.unit
    } yield u

    def touch(f: String)(implicit std: StdSinks) = for {
      process <- getScriptByName(s"${name}_touch").withArgs("file" -> f).executeLocally()
      u <- process.successfulExitCode.unit
    } yield u

    def delete(f: String)(implicit std: StdSinks) = for {
      process <- getScriptByName(s"${name}_delete").withArgs("file" -> f).executeLocally()
      u <- process.successfulExitCode.unit
    } yield u

    def copyFrom(src: String, srcEnv: Env, dst: String)(implicit std: StdSinks) = for {
      process <- getScriptByName(s"copy_from_${srcEnv}_to_$name")
        .withArgs("src" -> src, "dst" -> dst)
        .executeLocally()
      u <- process.successfulExitCode.unit
    } yield u

    def execute(wd: String, command: String, args: Seq[String], envVars: Map[String, String])(implicit std: StdSinks) = for {
      process <- getScriptByName(s"${name}_execute")
        .withArgs(
          "command" ->
            s"${envVars.map { case (k, v) => s"$k=${Escaper.Shell.escape(v)}" }.mkString(" ")} $command ${args.mkString(" ")}"
        )
        .executeLocally(wd)
      _ <- process.stdout.stream.run(std.out) <&> process.stderr.stream.run(std.err)
      exitCode <- process.exitCode
    } yield exitCode

  }
}
