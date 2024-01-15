package hypermake.collection

import scala.collection._

class PointedCaseCube(override val underlying: Map[Axis, PointedSet[String]]) extends CaseCube(underlying) {
  self =>

  def default: Case = Case(underlying.view.mapValues(_.default).toMap)

  override def assignments: Iterable[(Axis, PointedSet[String])] = underlying

  override def apply(a: Axis) = underlying(a)

  override def select(c: Case) = filterVars(a => !c.contains(a))

  override def filterVars(p: Axis => Boolean) = PointedCaseCube {
    underlying.view.filterKeys(p).toMap
  }

  def pointedSelectMany(cc: PointedCaseCube) = PointedCaseCube {
    self.underlying.map { case (a, ks) =>
      if (cc containsAxis a)
        a -> (ks intersect cc(a))
      else a -> ks
    }
  }

  def outerJoin(that: PointedCaseCube) = PointedCaseCube {
    val newVars = self.vars union that.vars
    newVars.view.map {
      case a if self.containsAxis(a) && that.containsAxis(a) =>
        a -> (self(a) intersect that(a))
      case a if self containsAxis a =>
        a -> self(a)
      case a =>
        a -> that(a)
    }.toMap
  }
}

object PointedCaseCube {

  def singleton = new PointedCaseCube(Map())

  def apply(underlying: Map[Axis, PointedSet[String]]) = new PointedCaseCube(underlying)

}
