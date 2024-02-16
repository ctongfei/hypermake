package hypermake.collection

import cats._
import hypermake.util._

import scala.collection._

/** A [[Tensor]] but with default value for each axis. As a result, each `PointedCube[A]` value has a single default value
  * `A`, similar to a pointed set.
  *
  * @tparam A
  *   Element type
  */
trait PointedTensor[+A] extends Tensor[A] {
  self =>

  import PointedTensor._

  def cases: PointedCaseTensor

  def pointedSelectMany(cc: PointedCaseTensor): PointedTensor[A] = new PointedTensor[A] {
    def cases = self.cases.pointedSelectMany(cc)

    def get(d: Case) = {
      if (d.assignments.forall { case (a, k) => cc(a) contains k })
        self.get(d)
      else None
    }
  }

  def product[B](that: PointedTensor[B]): PointedTensor[(A, B)] = productWith(that)((_, _))

  def productWith[B, C](that: PointedTensor[B])(f: (A, B) => C): PointedTensor[C] = new PointedTensor[C] {
    def cases = self.cases outerJoin that.cases

    def get(c: Case) = for {
      a <- self.get(c)
      b <- that.get(c)
    } yield f(a, b)
  }

  override def map[B](f: A => B): PointedTensor[B] = new Mapped(self, f)

  def flatMap[B](f: A => PointedTensor[B]): PointedTensor[B] = new PointedTensor[B] {
    def cases = self.cases outerJoin f(self.default).cases

    def get(c: Case) = for {
      a <- self.get(c)
      b <- f(a).get(c)
    } yield b
  }

  override def currySelectMany(cc: CaseTensor): Tensor[PointedTensor[A]] =
    curry(vars diff cc.vars).selectMany(cc)

  def reduceSelected[B](cc: CaseTensor, r: Tensor[A] => B): PointedTensor[B] =
    curry(cc.vars).map(r)

  override def curry(innerVars: Set[Axis]): PointedTensor[PointedTensor[A]] = new PointedTensor[PointedTensor[A]] {
    private[this] val outerVars = self.vars.filterNot(innerVars)

    def cases = self.cases.filterVars(outerVars)

    def get(c: Case) = {
      if (
        c.assignments.forall { case (a, k) =>
          (!(outerVars contains a) || ((outerVars contains a) && (cases(a) contains k)))
        }
      )
        Some(self.select(c))
      else None
    }
  }

  override def select(c: Case): PointedTensor[A] = new Selected(self, c)

  override def toString = s"[${cases.vars.mkString(", ")}] default = $default"

  /** Gets the default element of a cube.
    */
  def default: A = get(cases.default).get

}

object PointedTensor {

  def of[A](a: String, outerCase: (String, PointedTensor[A])*) =
    OfNestedMap(Axis(a), outerCase.toMap.pointed(outerCase.head._1))

  /** The `pure` operation of the PointedCube monad: construct a single value without parameterization.
    */
  case class Singleton[A](x: A) extends PointedTensor[A] {
    def cases = PointedCaseTensor(Map()) // single case

    def get(c: Case) = Some(x)

    override def default = x
  }

  case class OfMap[A](a: Axis, m: PointedMap[String, A]) extends PointedTensor[A] {
    def cases = PointedCaseTensor(Map(a -> m.keySet))

    def get(c: Case) = for {
      key <- c.get(a)
      a <- m.get(key)
    } yield a
  }

  case class OfNestedMap[A](a: Axis, outerCase: PointedMap[String, PointedTensor[A]]) extends PointedTensor[A] {
    val innerCases = outerCase.head._2.cases
    val innerAxes = innerCases.vars
    assert(outerCase.forall { case (_, c) => c.vars == innerAxes }) // make sure inner axes are identical

    def cases = PointedCaseTensor(Map(a -> outerCase.keySet) ++ innerCases.assignments)

    def get(c: Case) = for {
      innerCube <- outerCase.get(c(a))
      a <- innerCube.get(c)
    } yield a
  }

  /** `PointedCube` forms a commutative monad.
    */
  implicit object Monad extends StackSafeMonad[PointedTensor] with CommutativeMonad[PointedTensor] {
    def pure[A](x: A) = Singleton(x)

    def flatMap[A, B](fa: PointedTensor[A])(f: A => PointedTensor[B]) = fa flatMap f

    override def map[A, B](fa: PointedTensor[A])(f: A => B) = fa map f

    override def product[A, B](fa: PointedTensor[A], fb: PointedTensor[B]) = fa product fb
  }

  class Mapped[A, B](self: PointedTensor[A], f: A => B) extends Tensor.Mapped[A, B](self, f) with PointedTensor[B] {
    override def cases = self.cases
  }

  class Selected[A](self: PointedTensor[A], c: Case) extends PointedTensor[A] {
    def cases = self.cases.select(c)

    def get(d: Case) = self.get(c ++ d)
  }

}
