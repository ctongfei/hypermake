package forge.lang

import better.files._
import fastparse._
import zio._

import forge.core._
import forge.exception._
import forge.util._
import forge.lang.AST._


object Semantics {

  /**
   * Reads a Forge script while expanding all import statements.
   * @param f Script file to be read
   * @param ctx Context
   * @return A sequence of top-level definitions
   */
  def readFileToStmts(f: File)(implicit ctx: Context): Task[Seq[Stmt]] = {

    val stmts = for {
      content       <- Task { f.lines.mkString("\n") }
      stmts         <- parse(content, Parser.top(_)).asTask
      expandedStmts <- ZIO.collectAll(stmts.map {
        case ImportStmt(name) => for {
          f             <- ctx.resolveFile(name.value)
          importedStmts <- readFileToStmts(f)
        } yield importedStmts
        case stmt => Task(Seq(stmt))
      }).map(_.flatten)
    } yield expandedStmts

    stmts
  }

  /**
   * From definitions, deduces all declared axes.
   * @param stmts All definitions
   * @return All declared axes; fail if there is any axis mis-alignments.
   */
  def axes(stmts: Seq[Stmt]): IO[AxesAlignmentException, Map[String, Axis]] = {
    val axesOccurrences: Iterable[(String, Iterable[String])] =
      stmts.view.flatMap(_.recursiveChildren).collect {
        case DictLiteral(axis, assignments) => (axis.name, assignments.map(_._1.value))
      }
    val axes = axesOccurrences.groupBy(_._1).view.map { case (axisName, xs) =>
      val keys = xs.map(_._2.toSeq).toSeq
      val keys0 = keys.head
      if (keys.forall(_ == keys0))
        ZIO.succeed(Axis(axisName, keys0))
      else ZIO.fail(AxesAlignmentException(axisName, keys0, keys.find(_ != keys0).get))
    }
    ZIO.collectAll(axes) map { as => as.map(a => (a.name, a)).toMap }
  }


}
