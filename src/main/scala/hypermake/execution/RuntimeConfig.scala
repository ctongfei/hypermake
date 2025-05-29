package hypermake.execution

import java.io.{File => JFile, _}
import java.nio.file.{Files => JFiles, _}
import scala.jdk.CollectionConverters._

import better.files.File
import zio._

import hypermake.cli.CmdLineAST
import hypermake.cli.CmdLineAST._
import hypermake.util.git.cloneRepoToTempDir
import hypermake.util.printing._

/**
 * Encapsulates the runtime environment of a Hypermake run.
 * @param workDir Working directory
 * @param envVars Inherited environment variables from the parent process
 */
class RuntimeConfig private (
    val pipelineFile: String,
    val workDir: String,
    val shell: String,
    val envVars: Map[String, String],
    val definedVars: Map[String, String],
    val includePaths: Seq[String],
    val numParallelJobs: Int,
    val keepGoing: Boolean,
    val silent: Boolean,
    val verbose: Boolean,
    val yes: Boolean
) {

  /**
   * Environment variable containing paths where HyperMake resolves import statements;
   * akin to C's `CPATH` or Python's `PYTHONPATH`.
   */
  final val HYPERMAKE_PATH = "HYPERMAKE_PATH"

  lazy val paths =
    envVars.get(HYPERMAKE_PATH).map(_.split(JFile.pathSeparatorChar)).getOrElse(Array[String]())

  lazy val resolutionPaths = workDir +: paths

  private lazy val tempPath = JFiles.createTempDirectory("hypermake").toAbsolutePath

  lazy val tempDir = tempPath.toString

  lazy val isWindows = System.getProperty("os.name").toLowerCase contains "win"

  lazy val nullFile = if (isWindows) "NUL" else "/dev/null"

  def newTempFile(prefix: String = "", suffix: String = "") =
    JFiles.createTempFile(tempPath, prefix, suffix).toAbsolutePath.toString

  /**
   * Resolves a script file and read it into lines. The order of resolution is:
   *  - Current working directory
   *  - Paths in `HYPERMAKE_PATH` environment variable
   *  - Resource directory encoded in the JAR file
   * @param fn File name to resolve
   */
  def resolveFileThenRead(fn: String): Seq[String] = {
    resolutionPaths.collectFirst {
      case path if File(path, fn).exists => File(path, fn).lines.toSeq
    } getOrElse {
      // find the file in the resource directory
      getClass.getClassLoader.getResourceAsStream(fn) match {
        case null => throw new FileNotFoundException(s"HyperMake script ${O(fn)} not found.")
        case is   => scala.io.Source.fromInputStream(is).getLines().toSeq
      }
    }
  }

  def resolveModuleThenRead(name: String): Seq[String] = {
    val fn = name.replace('.', JFile.separatorChar) + ".hm"
    resolveFileThenRead(fn)
  }

  override def toString = {
    s"""pipelineFile = $pipelineFile
       |workDir = $workDir
       |shell = $shell
       |definedVars = ${definedVars.map { case (k, v) => s"$k = $v" }.mkString("{", ", ", "}")}
       |includePaths = ${includePaths.mkString("[", ", ", "]")}
       |numParallelJobs = $numParallelJobs
       |keepGoing = $keepGoing
       |silent = $silent
       |verbose = $verbose
       |yes = $yes
       |""".stripMargin
  }
}

object RuntimeConfig {

  private val defaultShell = "bash -e"

  def create(
      pipelineFile: String,
      definedVars: Map[String, String] = Map(),
      includePaths: Seq[String] = Seq(),
      includedGitRepos: Seq[String] = Seq(),
      shell: String = defaultShell,
      numParallelJobs: Int = 1,
      keepGoing: Boolean = false,
      silent: Boolean = false,
      verbose: Boolean = false,
      yes: Boolean = false
  ): RuntimeConfig = {
    val allGitRepos = ZIO.foreach(includedGitRepos)(cloneRepoToTempDir)
    val allClonedPath = Runtime.default.unsafeRun(allGitRepos)
    new RuntimeConfig(
      pipelineFile = pipelineFile,
      workDir = System.getProperty("user.dir"),
      envVars = System.getenv().asScala.toMap,
      definedVars = definedVars,
      shell = shell,
      includePaths = includePaths ++ allClonedPath,
      numParallelJobs = numParallelJobs,
      keepGoing = keepGoing,
      silent = silent,
      verbose = verbose,
      yes = yes
    )
  }

  def createFromCLIOptions(options: Seq[CmdLineAST.Opt], runOptions: Seq[CmdLineAST.RunOpt]) = {
    val workDir = System.getProperty("user.dir")
    val pipelineFile = options.collectFirst { case Opt.File(f) => f }.getOrElse {
      val files = File(workDir).list.filter(_.extension.contains(".hm")).toArray
      if (files.length == 1) files.head.pathAsString
      else throw new IllegalArgumentException("No pipeline file specified.")
    }
    create(
      pipelineFile = pipelineFile,
      includePaths = options.collect { case Opt.IncludeDir(f) => f },
      includedGitRepos = options.collect { case Opt.IncludeGit(r) => r },
      shell = options.collectFirst { case Opt.Shell(s) => s }.getOrElse(defaultShell),
      numParallelJobs = runOptions.collectFirst { case RunOpt.NumJobs(j) => j }.getOrElse(1),
      keepGoing = runOptions contains RunOpt.KeepGoing,
      silent = runOptions contains RunOpt.Silent,
      verbose = runOptions contains RunOpt.Verbose,
      yes = runOptions contains RunOpt.Yes
    )
  }

}
