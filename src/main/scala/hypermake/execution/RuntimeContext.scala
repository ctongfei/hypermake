package hypermake.execution

import better.files.File

import java.io.{File => JFile, _}
import java.nio.file.{Files => JFiles, _}
import scala.sys._


/**
 * Encapsulates the runtime context of a Forge run.
 * @param workDir Working directory
 * @param envVars Inherited environment variables from the parent process
 */
class RuntimeContext private(
                              val workDir: String,
                              val shell: String,
                              val envVars: Map[String, String],
                              val includePaths: Seq[String],
                              val numParallelJobs: Int,
                              val keepGoing: Boolean,
                              val dryRun: Boolean,
                              val silent: Boolean,
                              val yes: Boolean
                            ) {

  /**
   * Environment variable containing paths where HyperMake resolves import statements;
   * akin to C's `CPATH` or Python's `PYTHONPATH`.
   */
  final val HYPERMAKEPATH = "HYPERMAKEPATH"

  /**
   * Element separator for sequences of variables in a single shell string.
   */ // TODO: do we really want to inherit IFS from outside shell?
  final val IFS = envVars.getOrElse("IFS", " \t\n")  // Space&tab&newline is default for bash

  final val IFS_CHAR = IFS.head.toString

  lazy val SHELL = "bash"

  lazy val paths = envVars.get(HYPERMAKEPATH).map(_.split(JFile.pathSeparatorChar)).getOrElse(Array[String]())

  lazy val resolutionPaths = workDir +: paths

  private lazy val tempPath = JFiles.createTempDirectory("hypermake").toAbsolutePath

  lazy val tempDir = tempPath.toString

  def tempFile(prefix: String = "", suffix: String = "") =
    JFiles.createTempFile(tempPath, prefix, suffix).toAbsolutePath.toString

  /**
   * Resolves a Forge script file from `HYPERMAKEPATH`.
   * @param fn File name to resolve
   * @return The file
   */
  def resolveFile(fn: String): File = {
    resolutionPaths.collectFirst {
      case path if File(path, fn).exists => File(path, fn)
    }.getOrElse(throw new FileNotFoundException(s"HyperMake script $fn not found."))
  }

  def _println(s: String): Unit = {
    if (!silent) println(s)
  }

}

object RuntimeContext {

  def create(
              includePaths: Seq[String] = Seq(),
              shell: String = "bash",
              numParallelJobs: Int = 1,
              keepGoing: Boolean = false,
              dryRun: Boolean = false,
              silent: Boolean = false,
              yes: Boolean = false
            ): RuntimeContext =
    new RuntimeContext(  // TODO: additional cmdline args
      workDir = System.getProperty("user.dir"),
      envVars = env,
      shell = shell,
      includePaths = includePaths,
      numParallelJobs = numParallelJobs,
      keepGoing = keepGoing,
      dryRun = dryRun,
      silent = silent,
      yes = yes
    )

}
