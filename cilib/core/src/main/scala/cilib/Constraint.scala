package cilib

import scalaz.NonEmptyList
import spire.algebra.Eq
import spire.math._
import spire.math.interval._
import spire.implicits._

final class ViolationCount(val count: Int) extends AnyVal
object ViolationCount {
  def apply(i: Int): Option[ViolationCount] =
    if (i >= 0) Some(new ViolationCount(i))
    else None

  val zero: ViolationCount = new ViolationCount(0)

  import scalaz.Order
  import scalaz.std.anyVal._
  implicit val violationOrder: Order[ViolationCount] = new Order[ViolationCount] {
    def order(x: ViolationCount, y: ViolationCount) =
      Order[Int].order(x.count, y.count)
  }

  import scalaz._
  implicit def violationMonoid[A]: Monoid[ViolationCount] =
    new Monoid[ViolationCount] {
      def zero = ViolationCount.zero
      def append(f1: ViolationCount, f2: => ViolationCount) =
        ViolationCount(f1.count + f2.count).getOrElse(zero)
    }
}

abstract class ConstraintFunction[A] {
  def apply(a: NonEmptyList[A]): Double
}

object ConstraintFunction {
  private final case class VectorConstraint[A](f: NonEmptyList[A] => Double)
      extends ConstraintFunction[A] {
    def apply(a: NonEmptyList[A]) =
      f(a)
  }
  private final case class EvalConstraint[A](eval: Eval[NonEmptyList, A])
      extends ConstraintFunction[A] {
    def apply(a: NonEmptyList[A]) =
      eval.run(a) match {
        case Feasible(v)    => v
        case Adjusted(_, v) => v
        case Infeasible(v)  => -v
      }
  }

  def fromVector[A](f: NonEmptyList[A] => Double): ConstraintFunction[A] =
    VectorConstraint(f)

  def fromEval[A](e: Eval[NonEmptyList, A]): ConstraintFunction[A] =
    EvalConstraint(e)
}

sealed abstract class Constraint[A] {
  def f: ConstraintFunction[A]
}
final case class LessThan[A](v: Double, f: ConstraintFunction[A]) extends Constraint[A]
final case class LessThanEqual[A](v: Double, f: ConstraintFunction[A]) extends Constraint[A]
final case class Equal[A](v: Double, f: ConstraintFunction[A]) extends Constraint[A]
final case class InInterval[A](interval: Interval[Double], f: ConstraintFunction[A])
    extends Constraint[A]
final case class GreaterThan[A](v: Double, f: ConstraintFunction[A]) extends Constraint[A]
final case class GreaterThanEqual[A](v: Double, f: ConstraintFunction[A]) extends Constraint[A]

object Constraint {

  def eval[A](x: NonEmptyList[A], c: Constraint[A]): Double =
    c.f(x)

//  def constrain[M[_]](ma: M[Eval[Double]], cs: List[Constraint[Double,Double]])(implicit M: Functor[M]) =
//    M.map(ma)(_.constrainBy(cs))
  private val ev = Eq[Double]

  def violationMagnitude[A](beta: Double,
                            eta: Double,
                            constraints: List[Constraint[A]],
                            cs: NonEmptyList[A]): Double =
    constraints
      .map(_ match {
        case LessThan(v, f) =>
          val v2 = f(cs)
          if (v2 < v) 0.0
          else math.pow(math.abs(v2.toDouble - v.toDouble), beta) + eta
        case LessThanEqual(v, f) =>
          val v2 = f(cs)
          if (v2 <= v) 0.0
          else math.pow(math.abs(v2.toDouble - v.toDouble), beta) + eta
        case Equal(v, f) =>
          val v2 = f(cs)
          if (ev.eqv(v2, v)) 0.0 // Doubles are "equal" if they are equivalent using IEEE floats.
          else math.pow(math.abs(v2.toDouble - v.toDouble), beta) + eta
        case InInterval(i, f) =>
          val v2 = f(cs)
          val left = i.lowerBound match {
            case Closed(value) => value <= v2
            case Open(value)   => value < v2
            case Unbound()     => true
            case EmptyBound()  => false
          }
          val right = i.upperBound match {
            case Closed(value) => v2 <= value
            case Open(value)   => v2 < value
            case Unbound()     => true
            case EmptyBound()  => false
          }

          (left, right) match {
            case (true, true) => 0.0
            case (false, _) =>
              i.lowerBound match {
                case Closed(v) => math.pow(math.abs(v.toDouble - v2.toDouble), beta)
                case Open(v)   => math.pow(math.abs(v.toDouble - v2.toDouble), beta) + eta
                case _         => 0.0
              }
            case (_, false) =>
              i.upperBound match {
                case Closed(v) => math.pow(math.abs(v2.toDouble - v.toDouble), beta)
                case Open(v)   => math.pow(math.abs(v2.toDouble - v.toDouble), beta) + eta
                case _         => 0.0
              }
          }
        case GreaterThan(v, f) =>
          val v2 = f(cs)
          if (v2 > v) 0.0
          else math.pow(math.abs(v2.toDouble + v.toDouble), beta) + eta
        case GreaterThanEqual(v, f) =>
          val v2 = f(cs)
          if (v2 >= v) 0.0
          else math.pow(math.abs(v2.toDouble + v.toDouble), beta) + eta
      })
      .sum

  def violationCount[A](constraints: List[Constraint[A]], cs: NonEmptyList[A]): ViolationCount =
    ViolationCount(constraints.map(satisfies(_, cs)).filterNot(x => x).length)
      .getOrElse(ViolationCount.zero)

  def satisfies[A](constraint: Constraint[A], cs: NonEmptyList[A]): Boolean =
    constraint match {
      case LessThan(v, f)      => f(cs) < v
      case LessThanEqual(v, f) => f(cs) <= v
      case Equal(v, f)         => ev.eqv(f(cs), v)
      case InInterval(i, f) =>
        val v2 = f(cs)
        val c1 = i.lowerBound match {
          case Open(value)   => value < v2
          case Closed(value) => value <= v2
          case Unbound()     => true
          case EmptyBound()  => false
        }
        val c2 = i.upperBound match {
          case Open(value)   => v2 < value
          case Closed(value) => v2 <= value
          case Unbound()     => true
          case EmptyBound()  => false
        }
        c1 && c2
      case GreaterThan(v, f) => f(cs) > v
      case GreaterThanEqual(v, f) => {
        println(s"f(cs): ${f(cs)}")
        f(cs) >= v
      }
    }
}
