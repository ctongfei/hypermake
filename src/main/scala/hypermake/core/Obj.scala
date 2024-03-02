package hypermake.core

import hypermake.collection._
import hypermake.exception._
import hypermake.semantics._
import hypermake.util.DefaultMapBase

import scala.collection._

/** An object (a.k.a. module) that organizes definitions. */
class Obj {

  private[hypermake] val valueTable = mutable.HashMap[String, PointedTensor[Value]]()
  private[hypermake] val funcTable = mutable.HashMap[String, PointedFuncTensor]()
  private[hypermake] val taskTable = mutable.HashMap[String, PointedTaskTensor]()
  private[hypermake] val packageTable = mutable.HashMap[String, PointedPackageTensor]()
  private[hypermake] val planTable = mutable.HashMap[String, Plan]()
  private[hypermake] val classTable = mutable.HashMap[String, Cls]()
  private[hypermake] val objTable = mutable.HashMap[String, Obj]()

  def values = new PathMap[Obj, PointedTensor[Value]](this, _.objTable, _.valueTable)
  def functions = new PathMap[Obj, PointedFuncTensor](this, _.objTable, _.funcTable)
  def tasks = new PathMap[Obj, PointedTaskTensor](this, _.objTable, _.taskTable)
  def plans = new PathMap[Obj, Plan](this, _.objTable, _.planTable)
  def packages = new PathMap[Obj, PointedPackageTensor](this, _.objTable, _.packageTable)
  def classes = new PathMap[Obj, Cls](this, _.objTable, _.classTable)
  def objects = new PathMap[Obj, Obj](this, _.objTable, _.objTable)

  def packageAwareTasks(implicit ctx: Context): Map[String, PointedTaskTensor] =
    new DefaultMapBase[String, PointedTaskTensor] {
      def get(key: String) =
        try {
          val Array(packageName, packageFs) = key.split("@")
          val fs = FileSys(packageFs)
          Some(packages(packageName).on(fs))
        } catch { _ => taskTable.get(key) }
      def iterator = taskTable.iterator
    }

  def addDef(defn: Definition[_]): Unit = {
    val Definition(path, value) = defn
    val name = path.last
    val target = path.init.components.foldLeft(this) { (o, p) =>
      if (o.objTable contains p) o.objects(p)
      else {
        val newObj = new Obj
        o.objTable += p -> newObj
        newObj
      }
    }
    value match {
      case value: PointedTaskTensor =>
        if (target.taskTable.contains(name)) throw DuplicateDefinitionException("Task", name)
        else target.taskTable += name -> value
      case value: PointedFuncTensor =>
        if (target.funcTable.contains(name)) throw DuplicateDefinitionException("Function", name)
        else target.funcTable += name -> value
      case value: PointedPackageTensor =>
        if (target.packageTable.contains(name)) throw DuplicateDefinitionException("Package", name)
        else target.packageTable += name -> value
      case value: PointedTensor[Value] =>
        if (target.valueTable.contains(name)) throw DuplicateDefinitionException("Value", name)
        else target.valueTable += name -> value
      case value: Plan =>
        if (target.planTable.contains(name)) throw DuplicateDefinitionException("Plan", name)
        else target.planTable += name -> value
      case value: Cls =>
        if (target.classTable.contains(name)) throw DuplicateDefinitionException("Class", name)
        else target.classTable += name -> value
      case value: Obj =>
        if (target.objTable.contains(name)) throw DuplicateDefinitionException("Object", name)
        else target.objTable += name -> value
    }
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
    valueTable.map { case (k, v) => Definition(Path(List(k)), v) }.toSeq ++
      funcTable.map { case (k, v) => Definition(Path(List(k)), v) }.toSeq ++
      taskTable.map { case (k, v) => Definition(Path(List(k)), v) }.toSeq ++
      packageTable.map { case (k, v) => Definition(Path(List(k)), v) }.toSeq ++
      planTable.map { case (k, v) => Definition(Path(List(k)), v) }.toSeq ++
      classTable.map { case (k, v) => Definition(Path(List(k)), v) }.toSeq ++
      objTable.map { case (k, v) => Definition(Path(List(k)), v) }.toSeq
  }

}

object Obj {

  def fromDefs(defs: Iterable[Definition[_]]): Obj = {
    val obj = new Obj
    defs.foreach(obj.addDef)
    obj
  }

}
