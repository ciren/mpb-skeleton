package cilib

import scalaz._
import Scalaz._

abstract class Candidate[A] {
  import Candidate.{VectorCandidate}

  def map[B](f: A => B): Candidate[B] =
    this match {
      case VectorCandidate(x) => VectorCandidate(x.map(f))
    }

  def zip[B](other: Candidate[B]): Candidate[(A, B)] =
    (this, other) match {
      case (VectorCandidate(a), VectorCandidate(b)) => VectorCandidate(a.zip(b))
    }

  def traverse[G[_]: Applicative, B](f: A => G[B]): G[Candidate[B]] = ???
}

object Candidate {
  final case class VectorCandidate[A](x: NonEmptyList[A]) extends Candidate[A]
//final case class SetCandidate[A](a: A, rest: Set[A]) extends Candidate[A]

  implicit def equalCandidate[A: scalaz.Equal]: scalaz.Equal[Candidate[A]] =
    scalaz.Equal.equal[Candidate[A]]((a, b) =>
      (a, b) match {
        case (VectorCandidate(a), VectorCandidate(b)) =>
          a === b
    })

  implicit val foldableCandidate: Foldable1[Candidate] =
    new Foldable1[Candidate] {
      def foldMap1[A, B](fa: Candidate[A])(f: A => B)(implicit F: Semigroup[B]): B =
        fa match {
          case VectorCandidate(xs) =>
            xs.foldMap1(f)
        }

      def foldMapRight1[A, B](fa: Candidate[A])(z: A => B)(f: (A, => B) => B): B =
        fa match {
          case VectorCandidate(xs) =>
            xs.foldMapRight1(z)(f)
        }
    }
}
