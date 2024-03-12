package hypermake.collection

import scala.collection._

import cats.kernel._

import hypermake.util._

class PointedShape(override val underlying: Map[Axis, PointedSet[String]])
    extends Shape(underlying) {
  self =>

  def default: Case = Case(underlying.view.mapValues(_.default).toMap)

  override def assignments: Iterable[(Axis, PointedSet[String])] = underlying

  override def apply(a: Axis) = underlying(a)

  override def select(c: Case) = filterVars(a => !c.contains(a))

  override def filterVars(p: Axis => Boolean) = PointedShape {
    underlying.view.filterKeys(p).toMap
  }

  def pointedSelectMany(cc: PointedShape) = PointedShape {
    self.underlying.map { case (a, ks) =>
      if (cc containsAxis a)
        a -> (ks intersect cc(a))
      else a -> ks
    }
  }

  def outerJoin(that: PointedShape) = PointedShape {
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

  override def make[A](f: Case => A): PointedTensor[A] = new PointedTensor[A] {
    def shape = self
    def get(indices: Case) = Some(f(indices))
  }

  def equals(that: PointedShape) = {
    (self.vars equals that.vars) && self.vars.forall(a => self(a) equals that(a))
  }

}

object PointedShape {

  def singleton = new PointedShape(Map())

  def apply(underlying: Map[Axis, PointedSet[String]]) = new PointedShape(underlying)

  implicit object BoundedSemilattice extends BoundedSemilattice[PointedShape] {
    def empty = singleton
    def combine(x: PointedShape, y: PointedShape) = x outerJoin y
  }

  implicit object PartialOrder extends PartialOrder[PointedShape] {
    override def eqv(x: PointedShape, y: PointedShape) =
      (x.vars equals y.vars) && x.vars.forall(a => x(a) == y(a))

    def partialCompare(x: PointedShape, y: PointedShape) =
      if (eqv(x, y)) 0.0
      else if ((x.vars subsetOf y.vars) && x.vars.forall(a => x(a) equals y(a))) -1.0
      else if ((y.vars subsetOf x.vars) && y.vars.forall(a => y(a) equals x(a))) 1.0
      else Double.NaN

  }

}
