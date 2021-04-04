package hypermake.core

import scala.collection._
import cats._

/**
 * A value of type `A` parameterized by arbitrary string-keyed string values (i.e., a string-to-string map).
 * This is a ''non-pointed'' version of [[PointedCube]].
 * @tparam A Element type of the cube
 */
trait Cube[+A] { self =>
  import Cube._

  /**
   * The map of all cases of this cube:
   * with the key being the axis identifier and the value being the set of values it can take.
   */
  def cases: CaseCube

  def get(indices: Case): Option[A]

  def vars = cases.vars

  def apply(indices: (Name, String)*): A = get(Case.from(indices: _*)).get

  /**
   * ([S..., T...] => A) => ([S...] => [T...] => A)
   */
  def curry(innerAxes: Set[Name]): Cube[Cube[A]] = new Curried(self, innerAxes)

  /**
   * Selects a sub-cube based on the given indices. All indexed axes disappear in the returning cube.
   */
  def select(c: Case): Cube[A] = new Selected(self, c)

  /**
   * Selects a sub-cube based on the given indices set. Indexed axes are retained in the returning cube.
   */
  def selectMany(cc: CaseCube): Cube[A] = new SelectedMany(self, cc)

  def currySelectMany(cc: CaseCube): Cube[Cube[A]] =
    curry(cc.vars).map(_.selectMany(cc))

  def productWith[B, C](that: Cube[B])(f: (A, B) => C): Cube[C] = new ProductWith(self, that, f)

  def product[B](that: Cube[B]): Cube[(A, B)] = productWith(that)((_, _))

  def map[B](f: A => B): Cube[B] = new Mapped(self, f)

  /**
   * Returns a stream of all indices of this cube.
   */
  def allCases: Iterable[Case] = cases.all

  /**
   * Returns a stream of all elements in this cube.
   */
  def allElements: Iterable[A] = allCases.view.map(i => get(i).get)

  def allPairs: Iterable[(Case, A)] = allCases.view.map(i => i -> get(i).get)

  /**
   * Applies a side effect to all elements in this cube.
   * @param f Operation to apply
   */
  def foreach[U](f: A => U): Unit = allElements foreach f

  override def toString = s"[${cases.vars.mkString(", ")}]"

}

object Cube {

  /**
   * `Cube` forms a commutative applicative functor.
   */
  implicit object Applicative extends CommutativeApplicative[Cube] {
    def pure[A](x: A) = PointedCube.Singleton(x)
    def ap[A, B](ff: Cube[A => B])(fa: Cube[A]) = ff.productWith(fa)(_(_))
    override def map[A, B](fa: Cube[A])(f: A => B) = fa map f
    override def product[A, B](fa: Cube[A], fb: Cube[B]) = fa product fb
  }

  class Mapped[A, B](self: Cube[A], f: A => B) extends Cube[B] {
    def cases = self.cases
    def get(c: Case) = (self get c) map f
  }

  class ProductWith[A, B, C](self: Cube[A], that: Cube[B], f: (A, B) => C) extends Cube[C] {
    val cases = self.cases outerJoin that.cases
    def get(c: Case) = for {
      a <- self get c
      b <- that get c
    } yield f(a, b)
  }

  class Curried[A](self: Cube[A], innerVars: Set[Name]) extends Cube[Cube[A]] {
    private[this] val outerVars = self.vars.filterNot(innerVars)
    val cases = self.cases.filterVars(outerVars)
    def get(c: Case) = {
      if (c.assignments.forall { case (a, k) => (outerVars contains a) && (cases(a) contains k) })
        Some(self.select(c))
      else None
    }
  }

  class Selected[A](self: Cube[A], c: Case) extends Cube[A] {
    def cases = self.cases.select(c)
    def get(d: Case) = self.get(c ++ d)
  }

  class SelectedMany[A](self: Cube[A], cc: CaseCube) extends Cube[A] {
    val cases = self.cases.selectMany(cc)
    def get(c: Case) = {
      if ((c.vars intersect cases.vars).forall { a => cc(a) contains c(a) })  // all indices are in the sliced indices
        self.get(c) else None
    }
  }

}
