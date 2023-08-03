package hypermake.collection

import scala.collection._
import cats._
import hypermake.util._

/**
 * A [[Cube]] but with default value for each axis.
 * As a result, each `PointedCube[A]` value has a single default value `A`, similar to a pointed set.
 *
 * @tparam A Element type
 */
trait PointedCube[+A] extends Cube[A] {
  self =>

  import PointedCube._

  def cases: PointedCaseCube

  /**
   * Gets the default element of a cube.
   */
  def default: A = get(cases.default).get

  override def select(c: Case): PointedCube[A] = new Selected(self, c)

  override def curry(innerVars: Set[Name]): PointedCube[PointedCube[A]] = new PointedCube[PointedCube[A]] {
    private[this] val outerVars = self.vars.filterNot(innerVars)

    def cases = self.cases.filterVars(outerVars)

    def get(c: Case) = {
      if (c.assignments.forall { case (a, k) =>
        (!(outerVars contains a) || ((outerVars contains a) && (cases(a) contains k)))
      })
        Some(self.select(c))
      else None
    }
  }

  def pointedSelectMany(cc: PointedCaseCube): PointedCube[A] = new PointedCube[A] {
    def cases = self.cases.pointedSelectMany(cc)

    def get(d: Case) = {
      if (d.assignments.forall { case (a, k) => cc(a) contains k })
        self.get(d) else None
    }
  }

  def productWith[B, C](that: PointedCube[B])(f: (A, B) => C): PointedCube[C] = new PointedCube[C] {
    def cases = self.cases outerJoin that.cases

    def get(c: Case) = for {
      a <- self.get(c)
      b <- that.get(c)
    } yield f(a, b)
  }

  def product[B](that: PointedCube[B]): PointedCube[(A, B)] = productWith(that)((_, _))

  override def map[B](f: A => B): PointedCube[B] = new Mapped(self, f)

  def flatMap[B](f: A => PointedCube[B]): PointedCube[B] = new PointedCube[B] {
    def cases = self.cases outerJoin f(self.default).cases

    def get(c: Case) = for {
      a <- self.get(c)
      b <- f(a).get(c)
    } yield b
  }

  override def currySelectMany(cc: CaseCube): Cube[PointedCube[A]] =
    curry(vars diff cc.vars).selectMany(cc)

  override def toString = s"[${cases.vars.mkString(", ")}] default = $default"

}

object PointedCube {

  /**
   * The `pure` operation of the PointedCube monad: construct a single value without parameterization.
   */
  case class Singleton[A](x: A) extends PointedCube[A] {
    def cases = PointedCaseCube(Map()) // single case

    def get(c: Case) = Some(x)

    override def default = x
  }

  case class OfMap[A](a: Name, m: PointedMap[String, A]) extends PointedCube[A] {
    def cases = PointedCaseCube(Map(a -> m.keySet))

    def get(c: Case) = for {
      key <- c.get(a)
      a <- m.get(key)
    } yield a
  }

  case class OfNestedMap[A](a: Name, outerCase: PointedMap[String, PointedCube[A]]) extends PointedCube[A] {
    val innerCases = outerCase.head._2.cases
    val innerAxes = innerCases.vars
    assert(outerCase.forall { case (_, c) => c.vars == innerAxes }) // make sure inner axes are identical

    def cases = PointedCaseCube(Map(a -> outerCase.keySet) ++ innerCases.assignments)

    def get(c: Case) = for {
      innerCube <- outerCase.get(c(a))
      a <- innerCube.get(c)
    } yield a
  }

  def of[A](a: String, outerCase: (String, PointedCube[A])*) =
    OfNestedMap(Name(a), outerCase.toMap.pointed(outerCase.head._1))

  /**
   * `PointedCube` forms a commutative monad.
   */
  implicit object Monad extends StackSafeMonad[PointedCube] with CommutativeMonad[PointedCube] {
    def pure[A](x: A) = Singleton(x)

    def flatMap[A, B](fa: PointedCube[A])(f: A => PointedCube[B]) = fa flatMap f

    override def map[A, B](fa: PointedCube[A])(f: A => B) = fa map f

    override def product[A, B](fa: PointedCube[A], fb: PointedCube[B]) = fa product fb
  }

  class Mapped[A, B](self: PointedCube[A], f: A => B) extends Cube.Mapped[A, B](self, f) with PointedCube[B] {
    override def cases = self.cases
  }

  class Selected[A](self: PointedCube[A], c: Case) extends PointedCube[A] {
    def cases = self.cases.select(c)

    def get(d: Case) = self.get(c ++ d)
  }

}
