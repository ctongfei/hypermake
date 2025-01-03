package hypermake.util

import java.io.{File => JFile}
import java.net.URI
import java.nio.file.{Files => JFiles}

import zio.process.{Command, ProcessOutput}

package object git {

  /**
   * Clones a git repository to a temporary directory and returns the path to the directory.
   * @param repo the git URL of the repository. May contain a branch name or commit hash as
   *             the fragment part of the URL.
   */
  def cloneRepoToTempDir(repo: String): HIO[String] = {
    val uri = new URI(repo)
    val fragment = Option(uri.getFragment)
    // remove the fragment part of the URL, return string
    val base = if (fragment.isDefined) repo.substring(0, repo.indexOf('#')) else repo
    val tempDir = JFiles.createTempDirectory("hypermake").toAbsolutePath.toString
    val std = StdSinks.default
    val clone = Command("git", "clone", base, tempDir)
      .stderr(ProcessOutput.Pipe)
      .stdout(ProcessOutput.Pipe)
    val cloneEff = for {
      proc <- clone.run
      _ <- proc.stdout.stream.run(std.out) <&> proc.stderr.stream.run(std.err)
      u <- proc.successfulExitCode
    } yield u
    val checkout = Command("git", "checkout", fragment.getOrElse("main"))
      .workingDirectory(new JFile(tempDir))
      .stderr(ProcessOutput.Pipe)
      .stdout(ProcessOutput.Pipe)
    val checkoutEff = (for {
      proc <- checkout.run
      _ <- proc.stdout.stream.run(std.out) <&> proc.stderr.stream.run(std.err)
      u <- proc.successfulExitCode
    } yield u).when(fragment.isDefined)
    (cloneEff *> checkoutEff).as(tempDir)
  }

}
