package hypermake.execution

import better.files.File
import hypermake.cli.CmdLineAST
import hypermake.cli.CmdLineAST.{Opt, RunOpt}

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

  lazy val paths = envVars.get(HYPERMAKEPATH).map(_.split(JFile.pathSeparatorChar)).getOrElse(Array[String]())

  lazy val resolutionPaths = workDir +: paths

  private lazy val tempPath = JFiles.createTempDirectory("hypermake").toAbsolutePath

  lazy val tempDir = tempPath.toString

  lazy val nullFile = if (System.getProperty("os.name").toLowerCase contains "win") "NUL" else "/dev/null"

  def tempFile(prefix: String = "", suffix: String = "") =
    JFiles.createTempFile(tempPath, prefix, suffix).toAbsolutePath.toString

  /**
   * Resolves a script file from `HYPERMAKEPATH`.
   * @param fn File name to resolve
   * @return The file
   */
  def resolveFile(fn: String): File = {
    resolutionPaths.collectFirst {
      case path if File(path, fn).exists => File(path, fn)
    }.getOrElse(throw new FileNotFoundException(s"HyperMake script $fn not found."))
  }

  override def toString = {
    s"""workDir = $workDir
       |shell = $shell
       |includePaths = ${includePaths.mkString("[", ", ", "]")}
       |numParallelJobs = $numParallelJobs
       |keepGoing = $keepGoing
       |silent = $silent
       |yes = $yes
       |""".stripMargin
  }
}

object RuntimeContext {

  private val defaultShell = "bash -e"

  def create(
              includePaths: Seq[String] = Seq(),
              shell: String = defaultShell,
              numParallelJobs: Int = 1,
              keepGoing: Boolean = false,
              silent: Boolean = false,
              yes: Boolean = false
            ): RuntimeContext =
    new RuntimeContext(
      workDir = System.getProperty("user.dir"),
      envVars = env,
      shell = shell,
      includePaths = includePaths,
      numParallelJobs = numParallelJobs,
      keepGoing = keepGoing,
      silent = silent,
      yes = yes
    )

  def createFromCLIOptions(options: Seq[CmdLineAST.Opt], runOptions: Seq[CmdLineAST.RunOpt]) = create(
    includePaths = options.collect { case Opt.Include(f) => f },
    shell = options.collectFirst { case Opt.Shell(s) => s }.getOrElse(defaultShell),
    numParallelJobs = runOptions.collectFirst { case RunOpt.NumJobs(j) => j }.getOrElse(1),
    keepGoing = runOptions contains RunOpt.KeepGoing,
    silent = runOptions contains RunOpt.Silent,
    yes = runOptions contains RunOpt.Yes
  )

}
