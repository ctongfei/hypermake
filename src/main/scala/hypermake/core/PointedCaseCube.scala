package hypermake.core

import scala.collection._

class PointedCaseCube(override val underlying: Map[Name, PointedSet[String]]) extends CaseCube(underlying) { self =>

  def default: Case = Case(underlying.view.mapValues(_.default).toMap)

  override def filterVars(p: Name => Boolean) = PointedCaseCube {
    underlying.view.filterKeys(p).toMap
  }

  override def assignments: Iterable[(Name, PointedSet[String])] = underlying

  override def apply(a: Name) = underlying(a)

  override def select(c: Case) = filterVars(a => !c.contains(a))

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

  def apply(underlying: Map[Name, PointedSet[String]]) = new PointedCaseCube(underlying)

}
