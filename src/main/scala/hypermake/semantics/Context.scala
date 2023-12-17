package hypermake.semantics

import scala.collection._
import io.circe.syntax._
import hypermake.collection._
import hypermake.core._
import hypermake.exception._
import hypermake.execution.RuntimeConfig
import hypermake.syntax.Def
import hypermake.util.Escaper._

/** Contexts kept for each parsing run.
  */
class Context(implicit val runtime: RuntimeConfig) {

  private[hypermake] var allCases: PointedCaseCube = PointedCaseCube.singleton
  private[hypermake] val localEnv: Env = new Env.Local()(this)

  private[hypermake] val valueTable = mutable.HashMap[Name, PointedCube[Value]]()
  private[hypermake] val globalValueTable = mutable.HashMap[Name, PointedCube[Value]]()
  private[hypermake] val funcTable = mutable.HashMap[Name, Func]()
  private[hypermake] val taskTable = mutable.HashMap[Name, PointedCubeTask]()
  private[hypermake] val packageTable = mutable.HashMap[Name, PointedCubePackage]()
  private[hypermake] val planTable = mutable.HashMap[Name, Plan]()
  private[hypermake] val envTable = mutable.HashMap[Name, Env](Name("local") -> localEnv)
  private[hypermake] val moduleTable = mutable.HashMap[Name, Module]()

  def values: Map[Name, PointedCube[Value]] = valueTable

  def globalValues: Map[Name, PointedCube[Value]] = globalValueTable

  def functions: Map[Name, Func] = funcTable

  def tasks: Map[Name, PointedCubeTask] = taskTable

  def plans: Map[Name, Plan] = planTable

  def packages: Map[Name, PointedCubePackage] = packageTable

  def modules: Map[Name, Module] = moduleTable

  def envs: Map[Name, Env] = envTable

  def getAxis(name: Name) = allCases.underlying.getOrElse(name, throw UndefinedException("Axis", name))

  def getValue(name: Name) = {
    valueTable.getOrElse(name, throw UndefinedException("Value", name))
  }

  def getGlobalValue(name: Name) = {
    globalValueTable.getOrElse(name, throw UndefinedException("Global value", name))
  }

  def getValueOpt(name: Name) = valueTable.get(name)

  def getGlobalValueOpt(name: Name) = globalValueTable.get(name)

  def getPackage(name: Name) =
    packageTable.getOrElse(name, throw UndefinedException("Package", name))

  def getPackageOpt(name: Name) = packageTable.get(name)

  def getFunc(name: Name) = funcTable.getOrElse(name, throw UndefinedException("Function", name))

  def getTask(name: Name) = taskTable.getOrElse(
    name, {
      try { // somePackage@someEnv
        val Array(packageName, packageEnv) = name.name.split("@")
        getPackage(Name(packageName)).on(Env(Name(packageEnv))(this))(this)
      } catch { _ => throw UndefinedException("Task", name) }
    }
  )

  def getPlan(name: Name) = planTable.getOrElse(name, throw UndefinedException("Plan", name))

  def getEnv(name: Name) = envTable.getOrElse(name, throw UndefinedException("Environment", name))

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

  def envOutputRoot(envName: Name): String = {
    val path = getValueOpt(Name(s"${envName}_root")).map(_.default.value).getOrElse("out")
    path
  }

}
