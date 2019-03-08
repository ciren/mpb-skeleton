package cilib
package example

import scalaz._
import Scalaz._
import scalaz.effect._

import cilib.pso._

import eu.timepit.refined.auto._

object QuantumPSO extends SafeApp {
  import PSO._
  import Lenses._
  import spire.implicits._

  import monocle._

  case class QuantumState(b: Position[Double], v: Position[Double], charge: Double)

  object QuantumState {
    implicit object QSMemory
        extends HasMemory[QuantumState, Double]
        with HasVelocity[QuantumState, Double]
        with HasCharge[QuantumState] {
      def _memory = Lens[QuantumState, Position[Double]](_.b)(b => a => a.copy(b = b))
      def _velocity = Lens[QuantumState, Position[Double]](_.v)(b => a => a.copy(v = b))
      def _charge = Lens[QuantumState, Double](_.charge)(b => a => a.copy(charge = b))
    }
  }

  def quantumPSO[S](w: Double,
                    c1: Double,
                    c2: Double,
                    cognitive: Guide[S, Double],
                    social: Guide[S, Double],
                    cloudR: (Position[Double], Position[Double]) => RVar[Double])(
      implicit C: HasCharge[S],
      V: HasVelocity[S, Double],
      M: HasMemory[S, Double]
  ): NonEmptyList[Particle[S, Double]] => Particle[S, Double] => Step[Double, Particle[S, Double]] =
    collection =>
      x => {
        for {
          cog <- cognitive(collection, x)
          soc <- social(collection, x)
          v <- stdVelocity(x, soc, cog, w, c1, c2)
          p <- if (C._charge.get(x.state) < 0.01) stdPosition(x, v)
          else quantum(x.pos, cloudR(soc, cog), (_, _) => Dist.stdUniform).flatMap(replace(x, _))
          p2 <- evalParticle(p)
          p3 <- updateVelocity(p2, v)
          updated <- updatePBestBounds(p3)
        } yield updated
    }

  // Usage
  val domain = spire.math.Interval(0.0, 100.0) ^ 2

  val qpso =
    Iteration.sync(
      quantumPSO[QuantumState](0.729844,
                               1.496180,
                               1.496180,
                               Guide.pbest,
                               Guide.dominance(Selection.star),
                               (_, _) => RVar.pure(50.0)))
  val qpsoDist = Iteration.sync(
    quantumPSO[QuantumState](0.729844,
                             1.496180,
                             1.496180,
                             Guide.pbest,
                             Guide.gbest,
                             (_, _) => Dist.cauchy(0.0, 10.0)))

  def swarm =
    Position.createCollection(PSO.createParticle(x => Entity(QuantumState(x, x.zeroed, 0.0), x)))(
      domain,
      40)

  def pop =
    swarm
      .map(coll => {
        val C = implicitly[HasCharge[QuantumState]]
        val chargeLens = Lenses._state[QuantumState, Double].composeLens(C._charge)

        coll.zipWithIndex.map {
          case (current, index) => chargeLens.modify(z => if (index % 2 == 1) 0.1 else z)(current)
        }
      })
      .flatMap(RVar.shuffle)

  val comparison = Comparison.dominance(Max)

  override val runc: IO[Unit] = IO {}

}
