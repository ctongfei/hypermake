package hypermake.execution

import better.files.File
import hypermake.cli.CmdLineAST
import hypermake.cli.CmdLineAST._
import hypermake.util.printing._

import java.io.{File => JFile, _}
import java.nio.file.{Files => JFiles, _}
import scala.jdk.CollectionConverters._

/** Encapsulates the runtime environment of a Hypermake run.
  *
  * @param workDir
  *   Working directory
  * @param envVars
  *   Inherited environment variables from the parent process
  */
class RuntimeConfig private (
    val workDir: String,
    val shell: String,
    val envVars: Map[String, String],
    val definedVars: Map[String, String],
    val includePaths: Seq[String],
    val numParallelJobs: Int,
    val keepGoing: Boolean,
    val silent: Boolean,
    val yes: Boolean
) {

  /** Environment variable containing paths where HyperMake resolves import statements; akin to C's `CPATH` or Python's
    * `PYTHONPATH`.
    */
  final val HYPERMAKE_PATH = "HYPERMAKE_PATH"

  /** Element separator for sequences of variables in a single shell string.
    */
  // TODO: do we really want to inherit IFS from outside shell?
  final val IFS = envVars.getOrElse("IFS", " \t\n") // Space&tab&newline is default for bash

  final val IFS_CHAR = IFS.head.toString

  lazy val paths = envVars.get(HYPERMAKE_PATH).map(_.split(JFile.pathSeparatorChar)).getOrElse(Array[String]())

  lazy val bundledStdLibPath = this.getClass.getClassLoader.getResource("lib").getPath

  lazy val resolutionPaths = workDir +: paths :+ bundledStdLibPath

  private lazy val tempPath = JFiles.createTempDirectory("hypermake").toAbsolutePath

  lazy val tempDir = tempPath.toString

  lazy val nullFile = {
    if (System.getProperty("os.name").toLowerCase contains "win")
      "NUL"
    else "/dev/null"
  }

  def newTempFile(prefix: String = "", suffix: String = "") =
    JFiles.createTempFile(tempPath, prefix, suffix).toAbsolutePath.toString

  /** Resolves a script file from `HYPERMAKE_PATH`.
    *
    * @param fn
    *   File name to resolve
    * @return
    *   The file
    */
  def resolveFile(fn: String): File = {
    resolutionPaths.collectFirst {
      case path if File(path, fn).exists => File(path, fn)
    } getOrElse {
      val newFn = fn.replace('.', JFile.separatorChar) + ".hm"
      resolutionPaths.collectFirst {
        case path if File(path, newFn).exists => File(path, newFn)
      } getOrElse {
        throw new FileNotFoundException(s"Hypermake script ${O(fn)} not found.")
      }
    }
  }

  def resolveModule(name: String): File = {
    val fn = name.replace('.', JFile.separatorChar) + ".hm"
    resolveFile(fn)
  }

  override def toString = {
    s"""workDir = $workDir
       |shell = $shell
       |definedVars = ${definedVars.map { case (k, v) => s"$k = $v" }.mkString("{", ", ", "}")}
       |includePaths = ${includePaths.mkString("[", ", ", "]")}
       |numParallelJobs = $numParallelJobs
       |keepGoing = $keepGoing
       |silent = $silent
       |yes = $yes
       |""".stripMargin
  }
}

object RuntimeConfig {

  private val defaultShell = "bash -e"

  def create(
      definedVars: Map[String, String] = Map(),
      includePaths: Seq[String] = Seq(),
      shell: String = defaultShell,
      numParallelJobs: Int = 1,
      keepGoing: Boolean = false,
      silent: Boolean = false,
      yes: Boolean = false
  ): RuntimeConfig =
    new RuntimeConfig(
      workDir = System.getProperty("user.dir"),
      envVars = System.getenv().asScala.toMap,
      definedVars = definedVars,
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
