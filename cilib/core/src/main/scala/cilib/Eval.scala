package cilib

import scalaz._
import Scalaz._

trait Input[F[_]] {
  def toInput[A](a: NonEmptyList[A]): F[A]
}

sealed abstract class Eval[F[_], A] {
  import Eval._

  val F: Input[F]

  def run: F[A] => Fit //Double

  lazy val eval: NonEmptyList[A] => RVar[Objective[A]] =
    this match {
      case Unconstrained(f, _) =>
        (fa: NonEmptyList[A]) => RVar.pure { Objective.single(f(F.toInput(fa)), List.empty) }
      case Constrained(f, cs, _) =>
        (fa: NonEmptyList[A]) => RVar.pure {
          cs.toList.filter(c => !Constraint.satisfies(c, fa)) match {
            case Nil => Objective.single(f(F.toInput(fa)), List.empty)
            case xs =>
              val original = f(F.toInput(fa)).fold(
                penalty = adjusted => Infeasible(adjusted.adjust),
                valid = valid => Infeasible(valid.v),
                infeasible = identity
              )
              Objective.single(original, xs)
          }
        }
    }


  def constrain(cs: NonEmptyList[Constraint[A]]): Eval[F, A] =
    this match {
      case Unconstrained(f, _) => Constrained(f, cs, F)
      case Constrained(f, _, _) => Constrained(f, cs, F)
    }

  def unconstrain: Eval[F, A] =
    this match {
      case e @ Unconstrained(f, _) => e
      case Constrained(f, _, _) => Unconstrained(f, F)
    }

  def fold[B](unconstrained: (F[A] => Fit) => B,
              constrained: (F[A] => Fit, NonEmptyList[Constraint[A]]) => B): B =
    this match {
      case Unconstrained(f, i)   => unconstrained(f)
      case Constrained(f, cs, i) => constrained(f, cs)
    }

  // def map(g: Double => Double): Eval[F, A] =
  //   this match {
  //     case Unconstrained(f, i)   => Unconstrained((x: F[A]) => g(run(x)), i)
  //     case Constrained(f, cs, i) => Constrained((x: F[A]) => g(run(x)), cs, i)
  //   }
}

object Eval {
  private final case class Unconstrained[F[_], A](run: F[A] => Fit, F: Input[F]) extends Eval[F, A]
  private final case class Constrained[F[_], A](run: F[A] => Fit,
                                                cs: NonEmptyList[Constraint[A]],
                                                F: Input[F])
      extends Eval[F, A]

  def unconstrained[F[_], A](f: F[A] => Fit)(implicit F: Input[F]): Eval[F, A] =
    Unconstrained(f, F)

  def constrained[F[_], A](cs: NonEmptyList[Constraint[A]], f: F[A] => Fit)(
      implicit F: Input[F]): Eval[F, A] =
    Constrained(f, cs, F)
}

trait EvalInstances {
  import scalaz.{ICons, NonEmptyList}

  implicit val nelInput: Input[NonEmptyList] = new Input[NonEmptyList] {
    def toInput[A](a: NonEmptyList[A]): NonEmptyList[A] = a
  }

  implicit val pairInput: Input[Lambda[x => (x, x)]] =
    new Input[Lambda[x => (x, x)]] {
      def toInput[A](a: NonEmptyList[A]): (A, A) =
        a.list match {
          case ICons(a, ICons(b, _)) => (a, b)
          case _                     => sys.error("error producing a pair")
        }
    }
}
