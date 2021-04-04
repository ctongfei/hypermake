package hypermake.semantics

import hypermake.core._
import hypermake.exception._
import hypermake.semantics.Escaper.Percent

import scala.collection._

/**
 * Contexts kept for each parsing run.
 */
class ParsingContext(val runtime: RuntimeContext) {

  private[hypermake] var allCases: PointedCaseCube = PointedCaseCube.singleton
  val valueTable = mutable.HashMap[Name, PointedCube[Value]]()
  val funcTable = mutable.HashMap[Name, Func]()
  val taskTable = mutable.HashMap[Name, PointedCubeTask]()
  val planTable = mutable.HashMap[Name, Plan]()

  def values: Map[Name, PointedCube[Value]] = valueTable
  def functions: Map[Name, Func] = funcTable
  def tasks: Map[Name, PointedCubeTask] = taskTable
  def plans: Map[Name, Plan] = planTable

  def getAxis(name: Name) = allCases.underlying.getOrElse(name, throw UndefinedException("Axis", name))
  def getValue(name: Name) = valueTable.getOrElse(name, throw UndefinedException("Value", name))
  def getFunc(name: Name) = funcTable.getOrElse(name, throw UndefinedException("Function", name))
  def getTask(name: Name) = taskTable.getOrElse(name, throw UndefinedException("Task", name))
  def getPlan(name: Name) = planTable.getOrElse(name, throw UndefinedException("Plan", name))

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
}

object ParsingContext {



}
