package forge.core

import better.files.File
import forge.exception._
import zio._

import scala.sys._


class Context private(pwd: String, envs: Map[String, String]) {

  lazy val paths = envs.get("FORGE_PATH").map(_.split(':')).getOrElse(Array[String]())
  lazy val resolutionPaths = pwd +: paths :+ "/"

  /**
   * Resolves a Forge script file from `FORGE_PATH`.
   * @param fn File name to resolve
   * @return The file
   */
  def resolveFile(fn: String): IO[SourceNotFoundException, File] = ZIO.fromOption {
    resolutionPaths.collectFirst {
      case path if File(path, fn).exists => File(path, fn)
    }
  }.mapError(_ => SourceNotFoundException(fn))

}

object Context {

  def create(): Context = new Context(
    pwd = System.getProperty("user.dir"),
    envs = env
  )

}
