package hypermake.core

import hypermake.collection._
import hypermake.exception._
import hypermake.semantics._
import hypermake.util.DefaultMapBase

import scala.collection._

class Obj {

  private[hypermake] val valueTable = mutable.HashMap[String, PointedCube[Value]]()
  private[hypermake] val funcTable = mutable.HashMap[String, Func]()
  private[hypermake] val taskTable = mutable.HashMap[String, PointedCubeTask]()
  private[hypermake] val packageTable = mutable.HashMap[String, PointedCubePackage]()
  private[hypermake] val planTable = mutable.HashMap[String, Plan]()
  private[hypermake] val classTable = mutable.HashMap[String, Cls]()
  private[hypermake] val objTable = mutable.HashMap[String, Obj]()

  def values = new PathMap[Obj, PointedCube[Value]](this, _.objTable, _.valueTable)
  def functions = new PathMap[Obj, Func](this, _.objTable, _.funcTable)
  def tasks = new PathMap[Obj, PointedCubeTask](this, _.objTable, _.taskTable)
  def plans = new PathMap[Obj, Plan](this, _.objTable, _.planTable)
  def packages = new PathMap[Obj, PointedCubePackage](this, _.objTable, _.packageTable)
  def classes = new PathMap[Obj, Cls](this, _.objTable, _.classTable)
  def objects = new PathMap[Obj, Obj](this, _.objTable, _.objTable)

  def packageAwareTasks(implicit ctx: Context): Map[String, PointedCubeTask] =
    new DefaultMapBase[String, PointedCubeTask] {
      def get(key: String) =
        try {
          val Array(packageName, packageEnv) = key.split("@")
          val env = Env(packageEnv)
          Some(packages(packageName).on(env))
        } catch { _ => taskTable.get(key) }
      def iterator = taskTable.iterator
    }

  def addDef(defn: Definition[_]): Unit = defn match {
    case Definition(name, value: PointedCubeTask) =>
      if (taskTable.contains(name)) throw DuplicateDefinitionException("Task", name)
      else taskTable += name -> value
    case Definition(name, value: PointedCubePackage) =>
      if (packageTable.contains(name)) throw DuplicateDefinitionException("Package", name)
      else packageTable += name -> value
    case Definition(name, value: PointedCube[Value]) =>
      if (valueTable.contains(name)) throw DuplicateDefinitionException("Value", name)
      else valueTable += name -> value
    case Definition(name, value: Plan) =>
      if (planTable.contains(name)) throw DuplicateDefinitionException("Plan", name)
      else planTable += name -> value
    case Definition(name, value: Func) =>
      if (funcTable.contains(name)) throw DuplicateDefinitionException("Function", name)
      else funcTable += name -> value
    case Definition(name, value: Cls) =>
      if (classTable.contains(name)) throw DuplicateDefinitionException("Class", name)
      else classTable += name -> value
    case Definition(name, value: Obj) =>
      if (objTable.contains(name)) throw DuplicateDefinitionException("Object", name)
      else objTable += name -> value
  }

  def merge(other: Obj): Unit = {
    valueTable ++= other.valueTable
    funcTable ++= other.funcTable
    taskTable ++= other.taskTable
    packageTable ++= other.packageTable
    planTable ++= other.planTable
    classTable ++= other.classTable
    objTable ++= other.objTable
  }

  def defs: Seq[Definition[_]] = {
    valueTable.map { case (k, v) => Definition(k, v) }.toSeq ++
      funcTable.map { case (k, v) => Definition(k, v) }.toSeq ++
      taskTable.map { case (k, v) => Definition(k, v) }.toSeq ++
      packageTable.map { case (k, v) => Definition(k, v) }.toSeq ++
      planTable.map { case (k, v) => Definition(k, v) }.toSeq ++
      classTable.map { case (k, v) => Definition(k, v) }.toSeq ++
      objTable.map { case (k, v) => Definition(k, v) }.toSeq
  }

}

object Obj {
  def fromDefs(defs: Iterable[Definition[_]]): Obj = {
    val obj = new Obj
    defs.foreach(obj.addDef)
    obj
  }

}
