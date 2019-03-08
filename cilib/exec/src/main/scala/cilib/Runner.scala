package cilib
package exec

import scalaz._
import Scalaz._

import scalaz.stream._
import scalaz.concurrent.Task

import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.collection._

final case class Algorithm[A](name: String Refined NonEmpty, value: A)

final case class Problem[S, A](name: String Refined NonEmpty,
                               state: S,
                               env: Env,
                               eval: Eval[NonEmptyList, A])

final case class Progress[S, A, B] private (algorithm: String,
                                            problem: String,
                                            seed: Long,
                                            iteration: Int,
                                            env: Env,
                                            current: Problem[S, A],
                                            value: B)

object Runner {

  trait Iteration

  def repeat[M[_]: Monad, F[_], A](n: Int, alg: Kleisli[M, F[A], F[A]], collection: RVar[F[A]])(
      implicit M: MonadStep[M]): M[F[A]] =
    M.liftR(collection)
      .flatMap(coll =>
        (1 to n).toStream.foldLeftM[M, F[A]](coll) { (a, _) =>
          alg.run(a)
      })

  def staticAlgorithm[M[_]: Monad, /*F[_],*/ A](name: String Refined NonEmpty,
                                                a: Kleisli[M, A, A]) =
    Process.constant(Algorithm(name, a))

  def algorithm[M[_]: Monad, A, B](
      name: String Refined NonEmpty,
      config: A,
      f: A => Kleisli[M, B, B],
      updater: (A, Int @@ Iteration) => A): Process[Nothing, Algorithm[Kleisli[M, B, B]]] = {

    def go(current: A, iteration: Int): Process[Nothing, Algorithm[Kleisli[M, B, B]]] = {
      val next: Kleisli[M, B, B] = f(current)

      Process.emit(Algorithm(name, next)) ++
        go(updater(current, Tag[Int, Iteration](iteration)), iteration + 1)
    }

    go(config, 1)
  }

  def staticProblem[A](
      name: String Refined NonEmpty,
      eval: Eval[NonEmptyList, A]
  ): Process[Task, Problem[Unit, A]] =
    Process.constant(Problem(name, (), Unchanged, eval))

  def problem[S, A](name: String Refined NonEmpty,
                    env: Stream[Env],
                    state: RVar[S],
                    next: S => RVar[(S, Eval[NonEmptyList, A])])(
      rng: RNG
  ): Process[Task, Problem[S, A]] = {
    def go(s: S, c: Eval[NonEmptyList, A], e: Stream[Env], r: RNG): Process[Task, Problem[S, A]] =
      e match {
        case Stream.Empty => Process.empty
        case h #:: t =>
          h match {
            case Unchanged =>
              Process.emit(Problem(name, s, h, c)) ++ go(s, c, t, r)

            case Change =>
              val (rng2, (s1, c1)) = next(s).run(r)
              Process.emit(Problem(name, s1, h, c1)) ++ go(s1, c1, t, rng2)
          }
      }

    val (rng2, (s2, e)) = state.flatMap(next).run(rng)
    go(s2, e, env, rng2)
  }

  /**
    *  Interpreter for algorithm execution
    */
  def foldStep[F[_], S, A, B](cmp: Comparison,
                              rng: RNG,
                              collection: RVar[F[B]],
                              alg: Process[Task, Algorithm[Kleisli[Step[A, ?], F[B], F[B]]]],
                              env: Process[Task, Problem[S, A]],
                              onChange: (F[B], Eval[NonEmptyList, A]) => RVar[F[B]]): Process[
    Task,
    Progress[S, A, (Unit, F[B])]] = { // TODO: return type should be Progress[S, F[B], A]

    // Convert to a StepS with Unit as the state parameter
    val a: Process[Task, Algorithm[Kleisli[StepS[A, Unit, ?], F[B], F[B]]]] =
      alg.map(x => x.copy(value = Kleisli((a: F[B]) => StepS.pointS(x.value.run(a)))))

    foldStepS(cmp, (), rng, collection, a, env, onChange)
    //.map(x => x.copy(value = x.value._2))
  }

  def foldStepS[F[_], S1, S, A, B](
      cmp: Comparison,
      initialState: S,
      rng: RNG,
      collection: RVar[F[B]],
      alg: Process[Task, Algorithm[Kleisli[StepS[A, S, ?], F[B], F[B]]]],
      env: Process[Task, Problem[S1, A]],
      onChange: (F[B], Eval[NonEmptyList, A]) => RVar[F[B]])
    : Process[Task, Progress[S1, A, (S, F[B])]] = {

    def go(iteration: Int,
           r: RNG,
           current: F[B],
           state: S): Tee[Problem[S1, A],
                          Algorithm[Kleisli[StepS[A, S, ?], F[B], F[B]]],
                          Progress[S1, A, (S, F[B])]] =
      Process.awaitL[Problem[S1, A]].awaitOption.flatMap {
        case None => Process.halt
        case Some(prob @ Problem(problem, probState, e, eval)) =>
          Process.awaitR[Algorithm[Kleisli[StepS[A, S, ?], F[B], F[B]]]].awaitOption.flatMap {
            case None            => Process.halt
            case Some(algorithm) =>
//              println("running iteration:" + iteration.toString)
              val newConfig =
                Environment[A](cmp = cmp, eval = eval)

              val (r2, next) =
                e match {
                  case Unchanged => algorithm.value.run(current).run(state).run(newConfig).run(r)
                  case Change    =>
                    //val reset = F.map(current)(R.reset)
                    val (r3, updated) = onChange(current, newConfig.eval).run(r)
                    algorithm.value.run(updated).run(state).run(newConfig).run(r3)
                }

              next match {
                case -\/(error) => Process.fail(error)
                case \/-((newState, value)) =>
                  val progress =
                    Progress[S1, A, (S, F[B])](algorithm.name,
                                               problem,
                                               r2.seed,
                                               iteration,
                                               e,
                                               prob,
                                               (newState, value))
                  Process.emit(progress) ++ go(iteration + 1, r2, value, newState)
              }
          }
      }

    val (rng2, current) = collection.run(rng) // the collection of entities

    env.tee(alg)(go(1, rng2, current, initialState))
  }

  import com.sksamuel.avro4s._

  def measure[A, S, B, C](f: (Problem[S, A], B) => C)(
      implicit B: SchemaFor[C]): Process1[Progress[S, A, B], Measurement[C]] =
    process1.lift {
      case Progress(algorithm, problem, seed, iteration, env, prob, value) =>
        Measurement(algorithm, problem, iteration, env, seed, f(prob, value))
    }

  def measureWithState[A, S, S1, B, C](f: (S, Problem[S1, A], B) => C)(
      implicit B: SchemaFor[B]): Process1[Progress[S1, A, (S, B)], Measurement[C]] =
    process1.lift {
      case Progress(algorithm, problem, seed, iteration, env, eval, (state, value)) =>
        Measurement(algorithm, problem, iteration, env, seed, f(state, eval, value))
    }
}
