package hypermake.core

import scala.collection._

import hypermake.collection._
import hypermake.exception._
import hypermake.semantics._
import hypermake.util.DefaultMapBase

/** An object (a.k.a. module) that organizes definitions. */
class Obj(private[hypermake] var prefix: Path) {

  private[hypermake] val valueTable = mutable.HashMap[String, PointedTensor[Value]]()
  private[hypermake] val funcTable = mutable.HashMap[String, Func]()
  private[hypermake] val taskTable = mutable.HashMap[String, PointedTaskTensor]()
  private[hypermake] val packageTable = mutable.HashMap[String, PointedTaskTensor]()
  private[hypermake] val planTable = mutable.HashMap[String, Plan]()
  private[hypermake] val classTable = mutable.HashMap[String, Cls]()
  private[hypermake] val objTable = mutable.HashMap[String, Obj]()

  def values = PathMap[Obj, PointedTensor[Value]](this, _.objTable, _.valueTable)
  def functions = PathMap[Obj, Func](this, _.objTable, _.funcTable)
  def tasks = PathMap[Obj, PointedTaskTensor](this, _.objTable, _.taskTable)
  def plans = PathMap[Obj, Plan](this, _.objTable, _.planTable)
  def packages = PathMap[Obj, PointedTaskTensor](this, _.objTable, _.packageTable)
  def classes = PathMap[Obj, Cls](this, _.objTable, _.classTable)
  def objects = PathMap[Obj, Obj](this, _.objTable, _.objTable)

  def addDef(defn: Definition[_]): Unit = {
    val Definition(path, value) = defn
    val name = path.last
    val target = path.init.components.foldLeft(this) { (o, p) =>
      if (o.objTable contains p) o.objects(p)
      else {
        val newObj = new Obj(o.prefix / p)
        o.objTable += p -> newObj
        newObj
      }
    }
    value match {
      case value: PointedTaskTensor =>
        if (target.taskTable.contains(name)) throw DuplicateDefinitionException("Task", name)
        else target.taskTable += name -> value
      case value: Func =>
        if (target.funcTable.contains(name)) throw DuplicateDefinitionException("Function", name)
        else target.funcTable += name -> value
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

  def asDecorator: PointedDecoratorTensor = PointedDecoratorTensor.fromObj(this)

  def asService: PointedServiceTensor = PointedServiceTensor.fromObj(this)

}

object Obj {

  def fromDefs(prefix: Path, defs: Iterable[Definition[_]]): Obj = {
    val obj = new Obj(prefix)
    defs.foreach(obj.addDef)
    obj
  }

}
