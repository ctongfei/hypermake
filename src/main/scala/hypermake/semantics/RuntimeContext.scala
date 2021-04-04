package hypermake.semantics

import java.io.File.pathSeparatorChar
import java.io.FileNotFoundException

import better.files.File

import scala.sys._


/**
 * Encapsulates the runtime context of a Forge run.
 * @param workDir Working directory
 * @param envVars Inherited environment variables from the parent process
 */
class RuntimeContext private(
                              workDir: String,
                              envVars: Map[String, String],
                              buildPath: String,
                              includePaths: Seq[String],
                              numParallelJobs: Int,
                              keepGoing: Boolean,
                              dryRun: Boolean,
                              silent: Boolean,
                              yes: Boolean
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

  lazy val paths = envVars.get(HYPERMAKEPATH).map(_.split(pathSeparatorChar)).getOrElse(Array[String]())

  lazy val resolutionPaths = workDir +: paths

  lazy val localOutputRoot = workDir + "/out"

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

}

object RuntimeContext {

  def create(
              buildPath: String = "out",
              includePaths: Seq[String] = Seq(),
              numParallelJobs: Int = 1,
              keepGoing: Boolean = false,
              dryRun: Boolean = false,
              silent: Boolean = false,
              yes: Boolean = false
            ): RuntimeContext = new RuntimeContext(  // TODO: additional cmdline args
    workDir = System.getProperty("user.dir"),
    envVars = env,
    buildPath = buildPath,
    includePaths = includePaths,
    numParallelJobs = numParallelJobs,
    keepGoing = keepGoing,
    dryRun = dryRun,
    silent = silent,
    yes = yes
  )

}
