package hypermake.semantics

import scala.collection._
import io.circe.syntax._
import hypermake.collection._
import hypermake.core._
import hypermake.exception._
import hypermake.execution.RuntimeConfig
import hypermake.util.Escaper._

/** Contexts kept for each parsing run.
  */
class Context(implicit val runtime: RuntimeConfig) {

  private[hypermake] var root: Obj = new Obj
  private[hypermake] var allCases: PointedCaseTensor = PointedCaseTensor.singleton
  private[hypermake] val localEnv: Env = new Env.Local()(this)
  private[hypermake] val envTable = mutable.HashMap[String, Env]("local" -> localEnv)

  def getAxis(name: Axis) = allCases.underlying.getOrElse(name, throw UndefinedException("Axis", name.name))

  // TODO: env are now objects!
  def getEnv(name: String): Env = {
    envTable.getOrElse(name, throw UndefinedException("Environment", name))
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

  /** Encodes the arguments as a percent-encoded string. This is the name of the output directory in the file system.
    */
  def percentEncodedCaseString(args: Case) = {
    val clauses = canonicalizeCase(args).map { case (a, k) =>
      s"$a=${Percent.escape(k)}"
    }
    if (clauses.isEmpty) "default" else clauses.mkString("&") // URL style ID
  }

  def colorfulCaseString(args: Case) = {
    import fansi._
    val clauses = canonicalizeCase(args)
    if (clauses.isEmpty)
      Color.LightGreen("default").render
    else clauses.map { case (a, k) => s"${Color.Yellow(a)}: ${Bold.On(Color.LightGreen(k))}" }.mkString(", ")
  }

//  def envOutputRoot(envName: Name): String = {
//    val path = table.getValueOpt(Name(s"${envName}_root")).map(_.default.value).getOrElse("out")
//    path
//  }

}
