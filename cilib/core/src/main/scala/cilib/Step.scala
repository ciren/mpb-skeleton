package cilib

import scalaz.{Lens => _, _}
import Scalaz._

/**
  A `Step` is a type that models a single step / operation within a CI Algorithm.

  The general idea would be that you would compose different `Step`s
  to produce the desired algorithmic behaviour.

  Even though this is an initial pass at modeling the compuation of CI algorithms
  this way, it does provide a recursive, list-like composition allowing a multitude
  of different usages (or it is hoped to be so).

  `Step` is nothing more than a data structure that hides the details of a
  monad transformer stack which represents the algoritmic parts.
  */
sealed abstract class Step[A, B] {

  def run(env: Environment[A]): RVar[Exception \/ B] =
    this match {
      case Halt(r, e) =>
        e match {
          case None    => RVar.pure(\/.left[Exception, B](new Exception(r)))
          case Some(x) => RVar.pure(\/.left[Exception, B](new Exception(r, x)))
        }

      case Cont(f) =>
        f(env)
    }

  def map[C](f: B => C): Step[A, C] =
    this match {
      case Halt(r, e) => Halt(r, e)
      case Cont(_)    => Cont(e => run(e).map(_.map(f)))
    }

  def flatMap[C](f: B => Step[A, C]): Step[A, C] =
    this match {
      case Halt(r, e) => Halt(r, e)
      case Cont(_) =>
        Cont(env =>
          run(env).flatMap(_ match {
            case -\/(error) => RVar.pure(error.left[C])
            case \/-(value) => f(value).run(env)
          }))
    }
}

private final case class Cont[A, B](f: Environment[A] => RVar[Exception \/ B]) extends Step[A, B]
private final case class Halt[A, B](reason: String, error: Option[Exception]) extends Step[A, B]

private[cilib] sealed abstract class StepInstances0 {

  implicit def stepMonad[A]: Monad[Step[A, ?]] =
    new Monad[Step[A, ?]] {
      def point[B](a: => B): Step[A, B] =
        Step.pure(a)

      def bind[B, C](fa: Step[A, B])(f: B => Step[A, C]): Step[A, C] =
        fa.flatMap(f)
    }
}

private[cilib] sealed abstract class StepInstances extends StepInstances0 {

  implicit def stepReader[A]: MonadReader[Step[A, ?], Environment[A]] =
    new MonadReader[Step[A, ?], Environment[A]] {
      def point[B](a: => B) =
        Step.pure(a)

      def bind[B, C](fa: Step[A, B])(f: B => Step[A, C]): Step[A, C] =
        fa.flatMap(f)

      def ask: Step[A, Environment[A]] =
        Cont((e: Environment[A]) => RVar.pure(e.right[Exception]))

      def local[AA](f: Environment[A] => Environment[A])(fa: Step[A, AA]) =
        fa match {
          case Halt(r, e) => Halt(r, e)
          case Cont(f2)   => Cont((e: Environment[A]) => f2(f(e)))
        }
    }
}

object Step extends StepInstances {

  def fail[A, B](reason: String, error: Exception): Step[A, B] =
    Halt(reason, Option(error))

  def failString[A, B](reason: String): Step[A, B] =
    Halt(reason, None)

  @deprecated("This method has been deprecated, use pure instead, it is technically more accurate",
              "2.0.2")
  def point[A, B](b: B): Step[A, B] =
    pure(b)

  def pure[A, B](b: B): Step[A, B] =
    Cont(_ => RVar.pure(b.right[Exception]))

  @deprecated("This method has been deprecated, use liftR instead, it is technically more accurate",
              "2.0.2")
  def pointR[A, B](a: RVar[B]): Step[A, B] =
    liftR(a)

  def liftR[A, B](a: RVar[B]): Step[A, B] =
    Cont(_ => a.map(_.right[Exception]))

  def liftR2[A, B](a: RVar[Exception \/ B]): Step[A, B] =
    Cont(_ => a)

  def withCompare[A, B](a: Comparison => B): Step[A, B] =
    Cont(env => RVar.pure(a.apply(env.cmp).right[Exception]))

  def withCompareR[A, B](f: Comparison => RVar[B]): Step[A, B] =
    Cont(env => f(env.cmp).map(_.right[Exception]))

  def eval[S, A](f: Position[A] => Position[A])(entity: Entity[S, A]): Step[A, Entity[S, A]] =
    evalP(f(entity.pos)).map(p => Lenses._position.set(p)(entity))

  def evalP[A](pos: Position[A]): Step[A, Position[A]] =
    Cont { env =>
      Position.eval(env.eval.eval, pos).map(_.right[Exception])
    }
}

final case class StepS[A, S, B](run: StateT[Step[A, ?], S, B]) {
  def map[C](f: B => C): StepS[A, S, C] =
    StepS(run.map(f))

  def flatMap[C](f: B => StepS[A, S, C]): StepS[A, S, C] =
    StepS(run.flatMap(f(_).run))

  def zoom[S2](l: monocle.Lens[S2, S]): StepS[A, S2, B] =
    StepS(run.zoom(StepS.lensIso.reverseGet(l)))
}

private[cilib] sealed abstract class StepSInstances1 {

  implicit def stepSMonad[A, S]: Monad[StepS[A, S, ?]] =
    new Monad[StepS[A, S, ?]] {
      def point[B](a: => B): StepS[A, S, B] =
        StepS(StateT[Step[A, ?], S, B]((s: S) => Step.pure((s, a))))

      def bind[B, C](fa: StepS[A, S, B])(f: B => StepS[A, S, C]): StepS[A, S, C] =
        fa.flatMap(f)
    }
}

private[cilib] sealed abstract class StepSInstances0 extends StepSInstances1 {

  implicit def stepSMonadState[A, S]: MonadState[StepS[A, S, ?], S] =
    new MonadState[StepS[A, S, ?], S] {
      private val M = StateT.stateTMonadState[S, Step[A, ?]]

      def point[B](a: => B) = StepS(M.pure(a))

      def bind[B, C](fa: StepS[A, S, B])(f: B => StepS[A, S, C]): StepS[A, S, C] =
        fa.flatMap(f)

      def get: StepS[A, S, S] =
        StepS(M.get)

      def init = StepS(M.get)

      def put(s: S) =
        StepS(M.put(s))
    }
}

private[cilib] sealed abstract class StepSInstances extends StepSInstances0 {
  implicit def stepSMonadReader[A, S](implicit M: MonadReader[Step[A, ?], Environment[A]])
    : MonadReader[StepS[A, S, ?], Environment[A]] =
    new MonadReader[StepS[A, S, ?], Environment[A]] {
      def point[B](a: => B) =
        StepS.pure(a)

      def bind[B, C](fa: StepS[A, S, B])(f: B => StepS[A, S, C]): StepS[A, S, C] =
        fa.flatMap(f)

      def ask: StepS[A, S, Environment[A]] =
        StepS.pointS(M.ask)

      def local[AA](f: Environment[A] => Environment[A])(fa: StepS[A, S, AA]): StepS[A, S, AA] =
        StepS(StateT((s: S) => M.local(f)(fa.run.run(s))))
    }
}

object StepS extends StepSInstances {

  def lensIso[A, B]: monocle.Iso[scalaz.Lens[A, B], monocle.Lens[A, B]] =
    monocle.Iso[scalaz.Lens[A, B], monocle.Lens[A, B]]((s: scalaz.Lens[A, B]) =>
      monocle.Lens[A, B](s.get)(b => a => s.set(a, b)))((m: monocle.Lens[A, B]) =>
      scalaz.Lens.lensu[A, B]((a, b) => m.set(b)(a), m.get(_)))

  def pure[A, S, B](a: B): StepS[A, S, B] =
    StepS.pointS(Step.pure(a))

  @deprecated("This method has been deprecated, use liftR instead, it is technically more accurate",
              "2.0.2")
  def pointR[A, S, B](a: RVar[B]): StepS[A, S, B] =
    liftR(a)

  def liftR[A, S, B](a: RVar[B]): StepS[A, S, B] =
    StepS(StateT[Step[A, ?], S, B]((s: S) => Step.liftR(a).map((s, _))))

  def pointS[A, S, B](a: Step[A, B]): StepS[A, S, B] =
    StepS(StateT[Step[A, ?], S, B]((s: S) => a.map((s, _))))

  def liftK[A, S, B](a: Comparison => B): StepS[A, S, B] =
    pointS(Step.withCompare(a))

  def liftS[A, S, B](a: State[S, B]): StepS[A, S, B] =
    StepS(a.lift[Step[A, ?]])
}
