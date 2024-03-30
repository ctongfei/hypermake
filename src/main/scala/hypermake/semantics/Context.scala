package hypermake.semantics

import scala.collection._

import io.circe.syntax._

import hypermake.collection._
import hypermake.core._
import hypermake.exception._
import hypermake.execution.RuntimeConfig
import hypermake.util.Escaper._

/** Contexts kept for each parsing run. */
class Context(implicit val runtime: RuntimeConfig) {

  private[hypermake] val root: Obj = new Obj(Path.root)
  private[hypermake] val local: FileSys = new FileSys.Local()(this)
  private[hypermake] val fsTable = mutable.HashMap[String, FileSys]("local" -> local)
  private[hypermake] var allCases: PointedShape = PointedShape.singleton

  def getAxis(name: Axis) =
    allCases.underlying.getOrElse(name, throw UndefinedException("Axis", name.name))

  def getFs(name: String): FileSys = {
    fsTable.getOrElse(name, throw UndefinedException("File system", name))
  }

  def normalizeCase(c: Case): Case = {
    val assignments = c.assignments.collect {
      case (a, k) if allCases.containsAxis(a) && k != allCases.underlying(a).default => (a, k)
    }.toMap
    Case(assignments)
  }

  def canonicalizeCase(args: Case): Seq[(String, String)] = {
    val sortedArgs = args.underlying.toArray.sortBy(_._1)
    sortedArgs.collect {
      case (a, k) if getAxis(a).default != k =>
        a.name -> k
    }
  }

  def caseInJson(args: Case) =
    immutable.SeqMap.from(canonicalizeCase(args)).asJson.noSpaces

  def caseString(args: Case) =
    canonicalizeCase(args).map { case (a, k) => s"$a: $k" }.mkString(", ")

  def percentEncodedClauses(args: Case) = {
    val clauses = canonicalizeCase(args).map { case (a, k) =>
      s"$a=${Percent.escape(k)}"
    }
    clauses
  }

  /** Encodes the arguments as a percent-encoded string. This is the name of the output directory in the file system. */
  def percentEncodedCaseStringPath(args: Case) = {
    val clauses = percentEncodedClauses(args)
    if (clauses.isEmpty) "default" else clauses.mkString("&") // URL style ID
  }

  def percentEncodedCaseStringUrl(args: Case) = {
    val clauses = percentEncodedClauses(args)
    if (clauses.isEmpty) "" else clauses.mkString("&") // URL style ID
  }

  def colorfulCaseString(args: Case) = {
    import fansi._
    val clauses = canonicalizeCase(args)
    if (clauses.isEmpty)
      Color.LightGreen("default").render
    else
      clauses
        .map { case (a, k) => s"${Color.Yellow(a)}: ${Bold.On(Color.LightGreen(k))}" }
        .mkString(", ")
  }

//  def envOutputRoot(envName: Name): String = {
//    val path = table.getValueOpt(Name(s"${envName}_root")).map(_.default.value).getOrElse("out")
//    path
//  }

}
