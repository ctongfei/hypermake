package hypermake.semantics

import hypermake.collection._
import hypermake.core.{Env, Func, Package, Plan, PointedCubeTask, Value}
import hypermake.exception._
import hypermake.execution.RuntimeContext
import hypermake.util.Escaper._

import scala.collection._

/**
 * Contexts kept for each parsing run.
 */
class ParsingContext(implicit val runtime: RuntimeContext) {

  private[hypermake] var allCases: PointedCaseCube = PointedCaseCube.singleton
  private[hypermake] val localEnv: Env = new Env.Local()(this)

  val valueTable = mutable.HashMap[Name, PointedCube[Value]]()
  val funcTable = mutable.HashMap[Name, Func]()
  val taskTable = mutable.HashMap[Name, PointedCubeTask]()
  val packageTable = mutable.HashMap[Name, Package]()
  val planTable = mutable.HashMap[Name, Plan]()
  val envTable = mutable.HashMap[Name, Env](Name("local") -> localEnv)

  def values: Map[Name, PointedCube[Value]] = valueTable
  def functions: Map[Name, Func] = funcTable
  def tasks: Map[Name, PointedCubeTask] = taskTable
  def plans: Map[Name, Plan] = planTable
  def packages: Map[Name, Package] = packageTable
  def envs: Map[Name, Env] = envTable

  def getAxis(name: Name) = allCases.underlying.getOrElse(name, throw UndefinedException("Axis", name))
  def getValue(name: Name) = valueTable.getOrElse(name, throw UndefinedException("Value", name))
  def getValueOpt(name: Name) = valueTable.get(name)


  def getFunc(name: Name) = funcTable.getOrElse(name, throw UndefinedException("Function", name))
  def getTask(name: Name) = taskTable.getOrElse(name, throw UndefinedException("Task", name))
  def getPlan(name: Name) = planTable.getOrElse(name, throw UndefinedException("Plan", name))
  def getEnv(name: Name) = envTable.getOrElse(name, throw UndefinedException("Environment", name))

  def normalizeCase(c: Case): Case = {
    val assignments = c.assignments.collect {
      case (a, k) if allCases.containsAxis(a) && k != allCases.underlying(a).default => (a, k)
    }.toMap
    Case(assignments)
  }

  /**
   * Encodes the arguments as a percent-encoded string.
   */
  def escapedArgsString(args: Map[Name, String]) = {
    val sortedArgs = args.toArray.sortBy(_._1)
    val clauses = sortedArgs.collect {
      case (a, k) if getAxis(a).default != k =>
        s"$a=${Percent.escape(k)}"
    }
    if (clauses.isEmpty) "default" else clauses.mkString("&")  // URL style ID
  }

  def argsString(args: Map[Name, String]) = {
    val sortedArgs = args.toArray.sortBy(_._1)
    val clauses = sortedArgs.collect {
      case (a, k) if getAxis(a).default != k =>
        s"$a: ${Percent.escape(k)}"
    }
    clauses.mkString(", ")
  }

  def argsStringDefault(args: Map[Name, String]) = {
    import fansi._
    val sortedArgs = args.toArray.sortBy(_._1)
    val clauses = sortedArgs.collect {
      case (a, k) if getAxis(a).default != k =>
        s"${Color.Yellow(a.name)}: ${Bold.On(Color.LightGreen(k))}"
    }
    if (clauses.length == 0) Color.LightGreen("default").render else clauses.mkString(", ")
  }

  def envOutputRoot(env: Name): String =
    getValueOpt(Name(s"${env.name}_root")).map(_.default.value).getOrElse("out")

}

object ParsingContext {



}
