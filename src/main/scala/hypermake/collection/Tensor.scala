package hypermake.collection

import cats._
import cats.kernel.CommutativeMonoid

import scala.collection._

/** A value of type `A` parameterized by arbitrary string-keyed string values (i.e., a string-to-string map). This is a
  * ''non-pointed'' version of [[PointedTensor]].
  *
  * @tparam A
  *   Element type of the cube
  */
trait Tensor[+A] {
  self =>

  import Tensor._

  /** The map of all cases of this cube: with the key being the axis identifier and the value being the set of values it
    * can take.
    */
  def shape: Shape

  def get(indices: Case): Option[A]

  def apply(indices: (Axis, String)*): A = get(Case.from(indices: _*)).get

  /** Selects a sub-cube based on the given indices. All indexed axes disappear in the returning cube.
    */
  def select(c: Case): Tensor[A] = new Selected(self, c)

  def currySelectMany(cc: Shape): Tensor[Tensor[A]] =
    curry(vars diff cc.vars).selectMany(cc)

  def vars = shape.vars

  /** ([S..., T...] => A) => ([S...] => [T...] => A)
    */
  def curry(innerAxes: Set[Axis]): Tensor[Tensor[A]] = new Curried(self, innerAxes)

  /** Selects a sub-cube based on the given indices set. Indexed axes are retained in the returning cube.
    */
  def selectMany(cc: Shape): Tensor[A] = new SelectedMany(self, cc)

  def product[B](that: Tensor[B]): Tensor[(A, B)] = productWith(that)((_, _))

  def productWith[B, C](that: Tensor[B])(f: (A, B) => C): Tensor[C] = new ProductWith(self, that, f)

  def map[B](f: A => B): Tensor[B] = new Mapped(self, f)

  def allPairs: Iterable[(Case, A)] = allCases.view.map(i => i -> get(i).get)

  /** Returns a stream of all indices of this cube.
    */
  def allCases: Iterable[Case] = shape.all

  /** Applies a side effect to all elements in this cube.
    *
    * @param f
    *   Operation to apply
    */
  def foreach[U](f: A => U): Unit = allElements foreach f

  /** Returns a stream of all elements in this cube.
    */
  def allElements: Iterable[A] = allCases.view.map(i => get(i).get)

  override def toString = s"[${shape.vars.mkString(", ")}]"

}

object Tensor {

  /** `Cube` forms a commutative applicative functor.
    */
  implicit object Applicative extends CommutativeApplicative[Tensor] {
    def pure[A](x: A) = PointedTensor.Singleton(x)

    def ap[A, B](ff: Tensor[A => B])(fa: Tensor[A]) = ff.productWith(fa)(_(_))

    override def map[A, B](fa: Tensor[A])(f: A => B) = fa map f

    override def product[A, B](fa: Tensor[A], fb: Tensor[B]) = fa product fb

  }

  class Mapped[A, B](self: Tensor[A], f: A => B) extends Tensor[B] {
    def shape = self.shape

    def get(c: Case) = (self get c) map f
  }

  class ProductWith[A, B, C](self: Tensor[A], that: Tensor[B], f: (A, B) => C) extends Tensor[C] {
    val shape = self.shape outerJoin that.shape

    def get(c: Case) = for {
      a <- self get c
      b <- that get c
    } yield f(a, b)
  }

  class Curried[A](self: Tensor[A], innerVars: Set[Axis]) extends Tensor[Tensor[A]] {
    val shape = self.shape.filterVars(outerVars)
    private[this] val outerVars = self.vars.filterNot(innerVars)

    def get(c: Case) = {
      if (c.assignments.forall { case (a, k) => (outerVars contains a) && (shape(a) contains k) })
        Some(self.select(c))
      else None
    }
  }

  class Selected[A](self: Tensor[A], c: Case) extends Tensor[A] {
    def shape = self.shape.select(c)

    def get(d: Case) = self.get(c ++ d)
  }

  class SelectedMany[A](self: Tensor[A], cc: Shape) extends Tensor[A] {
    val shape = self.shape.selectMany(cc)

    def get(c: Case) = {
      if (
        (c.vars intersect shape.vars).forall { a => !cc.containsAxis(a) || cc(a).contains(c(a)) }
      ) // all indices are in the sliced indices
        self.get(c)
      else None
    }
  }

}
