package assignment

import scalaz.{Lens => _, _}
import Scalaz._
import monocle._
import Monocle._

import spire.algebra._
import spire.math.Interval
import spire.implicits.{eu => _, _}

import eu.timepit.refined.api._
import eu.timepit.refined.numeric._
import eu.timepit.refined.auto._
import eu.timepit.refined.collection._

import scalaz.stream._
import scalaz.concurrent.Task

import cilib._
import cilib.exec._

object MPB {

  sealed abstract class PeakMovement
  final case object Linear extends PeakMovement
  final case object Circular extends PeakMovement
  final case object Random extends PeakMovement

  // This is the Type2 growth movement of peaks in the MPB
  sealed abstract class PeakGrowth
  final case object Upward extends PeakGrowth
  final case object Downward extends PeakGrowth
  final case object NoGrowth extends PeakGrowth

  final case class Rotation(angle: Double, axes: (Int, Int))

  final case class Config(
    width: Interval[Double],
    height: Interval[Double],
    movement: PeakMovement,
    growth: PeakGrowth,
    rotation: Option[Rotation]
  )

  final case class PeakCone(
      height: Double,
      width: Double,
      location: NonEmptyList[Double],
      shift: NonEmptyList[Double],
      domain: NonEmptyList[Interval[Double]],
      config: Config,
      update: PeakCone => RVar[PeakCone]
  ) {
    def eval(x: NonEmptyList[Double]) = {
      val sum = x.zip(location).map(a => (a._1 - a._2) * (a._1 - a._2)).foldLeft1(_ + _)
      math.max(0, height - width * math.sqrt(sum))
    }

    override def toString =
      s"PeakCone($location,$height,$width)"
  }

  // Definitions of the 27 "standard" environments
  val randomGrowth =
    RVar.choose(NonEmptyList(Upward, Downward))

  def A1L(domain: NonEmptyList[Interval[Double]]): RVar[PeakCone] =
    initPeak(domain,
             defaultConfig,
             standardModify(lambda = 1.0, changeSeverity = 5.0, hSeverity = 0.0, wSeverity = 1.0))

  def A1C(domain: NonEmptyList[Interval[Double]], rotationAxes: (Int, Int)) =
    initPeak(domain,
      defaultConfig.copy(rotation = Some(Rotation(0, rotationAxes))),
      functionRotationModify(62.0)
    )

  def A1R(domain: NonEmptyList[Interval[Double]]) =
    initPeak(domain,
             defaultConfig,
             standardModify(lambda = 0.0, changeSeverity = 5.0, hSeverity = 0.0, wSeverity = 1.0))

  def A2L(domain: NonEmptyList[Interval[Double]]) =
    for {
      g <- randomGrowth
      c <- initPeak(domain,
                    defaultConfig.copy(growth = g),
                    type2Modify(
                      wSeverity = 1.0,
                      p =>
                        linearPeakHeight(p, p.config.height, hSeverity = 7.0))) // get the height interval from a lens?
    } yield c

  def A2C(domain: NonEmptyList[Interval[Double]]) =
    for {
      g <- randomGrowth
      c <- initPeak(domain,
                    defaultConfig.copy(growth = g),
                    type2Modify(wSeverity = 1.0, p => cyclePeakHeight(p, hSeverity = 7.0)))
    } yield c // get the height interval from a lens?

  def A2R(domain: NonEmptyList[Interval[Double]]) =
    for {
      g <- randomGrowth
      c <- initPeak(
        domain,
        defaultConfig.copy(growth = g),
        type2Modify(
          wSeverity = 1.0,
          p => randomSize(p, severity = 7.0, _height, _.config.height).map(a => (a, NoGrowth))
        )) // get the height interval from a lens?
    } yield c

  def A3L(domain: NonEmptyList[Interval[Double]]) =
    initPeak(domain,
             defaultConfig,
             standardModify(lambda = 1.0, changeSeverity = 5.0, hSeverity = 7.0, wSeverity = 1.0))

  def A3C(domain: NonEmptyList[Interval[Double]], rotationAxes: (Int, Int)) =
    initPeak(
      domain,
      defaultConfig.copy(rotation = Some(Rotation(0, rotationAxes))),
      p =>
        standardModify(lambda = 0.0, changeSeverity = 0.0, hSeverity = 7.0, wSeverity = 1.0)(p)
          .flatMap(functionRotationModify(105.0))
    )

  def A3R(domain: NonEmptyList[Interval[Double]]) =
    initPeak(domain,
             defaultConfig,
             standardModify(lambda = 0.0, changeSeverity = 5.0, hSeverity = 7.0, wSeverity = 1.0))

  def C1L(domain: NonEmptyList[Interval[Double]]) =
    initPeak(domain,
             defaultConfig,
             standardModify(lambda = 1.0, changeSeverity = 5.0, hSeverity = 0.0, wSeverity = 1.0))

  def C1C(domain: NonEmptyList[Interval[Double]], rotationAxes: (Int, Int)) =
    initPeak(domain, defaultConfig.copy(rotation = Some(Rotation(0, rotationAxes))), functionRotationModify(105.0))

  def C1R(domain: NonEmptyList[Interval[Double]]) =
    initPeak(domain,
             defaultConfig,
             standardModify(lambda = 0.0, changeSeverity = 5.0, hSeverity = 0.0, wSeverity = 1.0))

  def C2L(domain: NonEmptyList[Interval[Double]]) =
    for {
      g <- randomGrowth
      c <- initPeak(
        domain,
        defaultConfig.copy(growth = g),
        type2Modify(wSeverity = 1.0, p => linearPeakHeight(p, p.config.height, hSeverity = 7.0)))
    } yield c

  def C2C(domain: NonEmptyList[Interval[Double]]) =
    for {
      g <- randomGrowth
      c <- initPeak(
        domain,
        defaultConfig.copy(growth = g),
        type2Modify(wSeverity = 1.0, p => linearPeakHeight(p, p.config.height, hSeverity = 7.0)))
    } yield c

  def C2R(domain: NonEmptyList[Interval[Double]]) =
    for {
      g <- randomGrowth
      c <- initPeak(
        domain,
        defaultConfig.copy(growth = g),
        type2Modify(
          wSeverity = 1.0,
          p => randomSize(p, severity = 7.0, _height, _.config.height).map(a => (a, NoGrowth))
        )) // get the height interval from a lens?
    } yield c

  def C3L(domain: NonEmptyList[Interval[Double]]) =
    initPeak(domain,
             defaultConfig,
             standardModify(lambda = 1.0, changeSeverity = 5.0, hSeverity = 7.0, wSeverity = 1.0))

  def C3C(domain: NonEmptyList[Interval[Double]], rotationAxes: (Int, Int)) =
    initPeak(
      domain,
      defaultConfig.copy(rotation = Some(Rotation(0, rotationAxes))),
      p =>
        standardModify(lambda = 0.0, changeSeverity = 0.0, hSeverity = 7.0, wSeverity = 1.0)(p)
          .flatMap(functionRotationModify(105.0))
    )

  def C3R(domain: NonEmptyList[Interval[Double]]) =
    initPeak(domain,
             defaultConfig,
             standardModify(lambda = 0.0, changeSeverity = 5.0, hSeverity = 7.0, wSeverity = 1.0))

  def P1L(domain: NonEmptyList[Interval[Double]]) =
    initPeak(domain,
             defaultConfig,
             standardModify(lambda = 1.0, changeSeverity = 1.0, hSeverity = 0.0, wSeverity = 0.05))

  def P1C(domain: NonEmptyList[Interval[Double]], rotationAxes: (Int, Int)) =
    initPeak(domain, defaultConfig.copy(rotation = Some(Rotation(0, rotationAxes))), functionRotationModify(314.0))

  def P1R(domain: NonEmptyList[Interval[Double]]) =
    initPeak(domain,
             defaultConfig,
             standardModify(lambda = 0.0, changeSeverity = 1.0, hSeverity = 0.0, wSeverity = 0.05))

  def P2L(domain: NonEmptyList[Interval[Double]]) =
    for {
      g <- randomGrowth
      c <- initPeak(
        domain,
        defaultConfig.copy(growth = g),
        type2Modify(wSeverity = 0.05, p => linearPeakHeight(p, p.config.height, hSeverity = 1.0)))
    } yield c

  def P2C(domain: NonEmptyList[Interval[Double]]) =
    for {
      g <- randomGrowth
      c <- initPeak(
        domain,
        defaultConfig.copy(growth = g),
        type2Modify(wSeverity = 0.05, p => linearPeakHeight(p, p.config.height, hSeverity = 1.0)))
    } yield c

  def P2R(domain: NonEmptyList[Interval[Double]]) =
    for {
      g <- randomGrowth
      c <- initPeak(
        domain,
        defaultConfig.copy(growth = g),
        type2Modify(
          wSeverity = 0.05,
          p => randomSize(p, severity = 1.0, _height, _.config.height).map(a => (a, NoGrowth))
        )) // get the height interval from a lens?
    } yield c

  def P3L(domain: NonEmptyList[Interval[Double]]) =
    initPeak(domain,
             defaultConfig,
             standardModify(lambda = 1.0, changeSeverity = 1.0, hSeverity = 1.0, wSeverity = 0.05))

  def P3C(domain: NonEmptyList[Interval[Double]]) =
    initPeak(
      domain,
      defaultConfig,
      p =>
        standardModify(lambda = 0.0, changeSeverity = 0.0, hSeverity = 1.0, wSeverity = 0.05)(p)
          .flatMap(functionRotationModify(314.0))
    )

  def P3R(domain: NonEmptyList[Interval[Double]]): RVar[PeakCone] =
    initPeak(domain,
             defaultConfig,
             standardModify(lambda = 0.0, changeSeverity = 1.0, hSeverity = 1.0, wSeverity = 0.05))

  val defaultConfig: Config =
    Config(width = Interval(1.0, 12.0),
           height = Interval(30.0, 70.0),
           movement = Random,
           growth = NoGrowth,
           rotation = None)

  def problem(
      name: String Refined NonEmpty,
      env: Stream[Env],
      nPeaks: Int Refined Positive,
      config: RVar[PeakCone]): RNG => Process[Task, Problem[NonEmptyList[PeakCone], Double]] =
    r =>
      Runner.problem(
        name,
        env,
        initPeaks(nPeaks, config),
        (peaks: NonEmptyList[PeakCone]) => MPB.modifyPeaks(peaks).map(x => (x, peakEval(x))))(r)

  val stdDomain = Interval(0.0, 100.0) ^ 5
  val staticChangeStream = Env.unchanging
  val abruptChangeStream = Env.frequency(100)
  val chaoticChangeStream = Env.frequency(20)
  val progressiveChangeStream = Env.frequency(20)
  val peakCount: Int Refined Positive = 10


  def rotationPlane(domain: NonEmptyList[Interval[Double]]): RVar[(Int, Int)] =
    RVar.sample(1, List.range(0, domain.size).combinations(2).toList)
      .map(_ match {
        case Some(list) =>
          list.head match {
            case a :: b :: Nil => (a, b)
            case _ => sys.error("Unable to determine the rotation plane")
          }
        case _ => sys.error("Unable to determine the rotation plane")
      })


  // Abrupt environments
  def probA1L(r: RNG) =
    problem("A1L", abruptChangeStream, peakCount, A1L(stdDomain)).apply(r)
  def probA1C(r: RNG) =
    problem("A1C",
      abruptChangeStream,
      peakCount,
      rotationPlane(stdDomain).flatMap(p => A1C(stdDomain, p))
    ).apply(r)

  def probA1R(r: RNG) =
    problem("A1R", abruptChangeStream, peakCount, A1R(stdDomain)).apply(r)
  def probA2L(r: RNG) =
    problem("A2L", abruptChangeStream, peakCount, A2L(stdDomain)).apply(r)
  def probA2C(r: RNG) =
    problem("A2C", abruptChangeStream, peakCount, A2C(stdDomain)).apply(r)
  def probA2R(r: RNG) =
    problem("A2R", abruptChangeStream, peakCount, A2R(stdDomain)).apply(r)
  def probA3L(r: RNG) =
    problem("A3L", abruptChangeStream, peakCount, A3L(stdDomain)).apply(r)
  def probA3C(r: RNG) =
    problem("A3C", abruptChangeStream, peakCount, rotationPlane(stdDomain).flatMap(x => A3C(stdDomain, x))).apply(r)
  def probA3R(r: RNG) =
    problem("A3R", abruptChangeStream, peakCount, A3R(stdDomain)).apply(r)

  // Chaotic environments
  def probC1L(r: RNG) =
    problem("C1L", chaoticChangeStream, peakCount, C1L(stdDomain)).apply(r)
  def probC1C(r: RNG) =
    problem("C1C", chaoticChangeStream, peakCount, rotationPlane(stdDomain).flatMap(x => C1C(stdDomain, x))).apply(r)
  def probC1R(r: RNG) =
    problem("C1R", chaoticChangeStream, peakCount, C1R(stdDomain)).apply(r)
  def probC2L(r: RNG) =
    problem("C2L", chaoticChangeStream, peakCount, C2L(stdDomain)).apply(r)
  def probC2C(r: RNG) =
    problem("C2C", chaoticChangeStream, peakCount, C2C(stdDomain)).apply(r)
  def probC2R(r: RNG) =
    problem("C2R", chaoticChangeStream, peakCount, C2R(stdDomain)).apply(r)
  def probC3L(r: RNG) =
    problem("C3L", chaoticChangeStream, peakCount, C3L(stdDomain)).apply(r)
  def probC3C(r: RNG) =
    problem("C3C", chaoticChangeStream, peakCount, rotationPlane(stdDomain).flatMap(x => C3C(stdDomain, x))).apply(r)
  def probC3R(r: RNG) =
    problem("C3R", chaoticChangeStream, peakCount, C3R(stdDomain)).apply(r)

  // Progressive environments
  def probP1L(r: RNG) =
    problem("P1L", progressiveChangeStream, peakCount, P1L(stdDomain)).apply(r)
  def probP1C(r: RNG) =
    problem("P1C", progressiveChangeStream, peakCount, rotationPlane(stdDomain).flatMap(x => P1C(stdDomain, x))).apply(r)
  def probP1R(r: RNG) =
    problem("P1R", progressiveChangeStream, peakCount, P1R(stdDomain)).apply(r)
  def probP2L(r: RNG) =
    problem("P2L", progressiveChangeStream, peakCount, P2L(stdDomain)).apply(r)
  def probP2C(r: RNG) =
    problem("P2C", progressiveChangeStream, peakCount, P2C(stdDomain)).apply(r)
  def probP2R(r: RNG) =
    problem("P2R", progressiveChangeStream, peakCount, P2R(stdDomain)).apply(r)
  def probP3L(r: RNG) =
    problem("P3L", progressiveChangeStream, peakCount, P3L(stdDomain)).apply(r)
  def probP3C(r: RNG) =
    problem("P3C", progressiveChangeStream, peakCount, P3C(stdDomain)).apply(r)
  def probP3R(r: RNG) =
    problem("P3R", progressiveChangeStream, peakCount, P3R(stdDomain)).apply(r)

  // Static environment
  def probSTA(r: RNG) =
    problem("STA", staticChangeStream, peakCount, initPeak(stdDomain, defaultConfig, RVar.pure(_)))

  /* Attempt to implment the moving peaks in such a way that it does not suck */
  def initPeaks[S](n: Int Refined Positive, initial: RVar[S]): RVar[NonEmptyList[S]] =
    (initial |@| initial.replicateM(n - 1)) { (a, b) =>
      NonEmptyList.nel(a, IList.fromList(b))
    }

  def initPeak(domain: NonEmptyList[Interval[Double]],
               config: Config,
               update: PeakCone => RVar[PeakCone]): RVar[PeakCone] =
    domain
      .traverse(Dist.uniform)
      .flatMap(x => {
        val height =
          Dist.stdUniform
            .map(_ * range(config.height) + lowerValue(config.height)) // TODO: Extract to function?
        val width =
          Dist.stdUniform
            .map(_ * range(config.width) + lowerValue(config.width)) // TODO: Extract into a common funtion

        val initialShift = domain.traverse(_ => RVar.next[Double])

        (height |@| width |@| initialShift) { (h, w, s) =>
          PeakCone(h, w, x, s /*x.map(_ => 1.0)*/, domain, config, update)
        }
      })

  // Can we not convert this to a state-like lens action?
  def standardModify(lambda: Double, changeSeverity: Double, hSeverity: Double, wSeverity: Double)(
      p: PeakCone): RVar[PeakCone] =
    for {
      l <- locationAndShift(p, lambda, changeSeverity)
      h <- randomSize(p, hSeverity, _height, _.config.height)
      w <- randomSize(p, wSeverity, _width, _.config.width)
    } yield p.copy(height = h, width = w, location = l._1, shift = l._2)

  def type2Modify(wSeverity: Double, height: PeakCone => RVar[(Double, PeakGrowth)])(
      p: PeakCone): RVar[PeakCone] =
    for {
      x <- height(p)
      w <- randomSize(p, wSeverity, _width, _.config.width)
    } yield p.copy(height = x._1, config = p.config.copy(growth = x._2), width = w)

  def functionRotationModify(rotationPeriod: Double)(p: PeakCone): RVar[PeakCone] =
    RVar.pure {
      p.copy(config = p.config.copy(rotation = p.config.rotation.map(r =>
        r.copy(angle = (r.angle + (2 * Math.PI / rotationPeriod)) % (2 * Math.PI)))))
    }

  // Question about the lambda and changeSeverity parameters.... state-like update???? lenses?
  def locationAndShift(
      p: PeakCone,
      lambda: Double,
      changeSeverity: Double): RVar[(NonEmptyList[Double], NonEmptyList[Double])] = {
    val r: RVar[NonEmptyList[Double]] = p.shift.traverse(_ => Dist.stdNormal)
    val term1: RVar[NonEmptyList[Double]] = r.map(_.map(_ * (1.0 - lambda) * changeSeverity))
    val linComb: RVar[NonEmptyList[Double]] =
      term1.map(t1 => {
        val term2: NonEmptyList[Double] = p.shift.map(_ * lambda)
        t1.zip(term2).map { case (a, b) => a + b }
      })

    val length: RVar[Double] =
      r.map(_r => {
        val squaredSum: Double =
          _r.zip(p.shift).foldLeft(0.0) { case (z, (a, b)) => z + ((a + b) * (a + b)) }
        math.sqrt(squaredSum)
      })

    val stepSize = length.map(l => changeSeverity / l)

    (stepSize |@| linComb |@| Dist.stdNormal |@| Dist.stdNormal) { (scalar, shift, s1, s2) =>
      shift
        .map(_ * scalar)
        .zip(p.location)
        .zip(p.domain)
        .map {
          case ((s, p), d) =>
            val trial = p + s

            if (d.contains(trial)) (trial, s)
            else if (lowerValue(d) > trial) (2.0 * lowerValue(d) - p - s, negate(s))
            else (2.0 * upperValue(d) - p - s, negate(s))
        }
        .unzip
    }
  }

  def negate[A](a: A)(implicit A: Ring[A]) =
    A.negate(a)

  // Needs better naming and use like a state lens action
  // Passing in a Lens[PeakCone, Double] could abstract which value is altered, as the algorithm remains the same?
  def randomSize(
      p: PeakCone,
      severity: Double,
      l: Lens[PeakCone, Double],
      interval: PeakCone => Interval[Double]
  ): RVar[Double] =
    Dist.stdNormal.map(s1 => {
      val i = interval(p)
      val change = s1 * severity
      val candidate = l.get(p) + change

      if (candidate < lowerValue(i)) 2.0 * lowerValue(i) - l.get(p) - change
      else if (candidate > upperValue(i)) 2.0 * upperValue(i) - l.get(p) - change
      else candidate
    })

  def linearPeakHeight(p: PeakCone,
                       heightInterval: Interval[Double],
                       hSeverity: Double): RVar[(Double, PeakGrowth)] =
    Dist.stdUniform.map(u1 => {
      val _range = range(heightInterval)
      val direction = _peakGrowth.get(p)
      val change = direction match {
        case Upward   => (upperValue(p.config.height) - p.height) / _range * hSeverity * u1
        case Downward => (p.height - lowerValue(p.config.height)) / _range * hSeverity * u1
        case _ =>
          sys.error("Illegal MPB cycle direction config.") //impossible to happen, but just in case.
      }
      val candidate = p.height + change

      if (candidate < lowerValue(p.config.height)) (lowerValue(p.config.height), direction)
      else if (candidate > upperValue(p.config.height)) (upperValue(p.config.height), direction)
      else (candidate, direction)
    })

  def cyclePeakHeight(p: PeakCone, hSeverity: Double): RVar[(Double, PeakGrowth)] =
    Dist.stdNormal.map(s1 => {
      val change = p.config.growth match {
        case Upward   => Math.abs(s1) * hSeverity
        case Downward => -1.0 * Math.abs(s1) * hSeverity
        case _ =>
          sys.error("Illegal MPB cycle direction config.") //impossible to happen, but just in case.
      }
      val candidate = p.height + change

      // FIXME!!!!! The lookup of the height interval
      if (candidate < lowerValue(p.config.height))
        (2.0 * lowerValue(p.config.height) - p.height - change, Upward)
      else if (candidate > upperValue(p.config.height))
        (2.0 * upperValue(p.config.height) - p.height - change, Downward)
      else (candidate, p.config.growth)
    })

  /*
   The provided function `f` applies the _kind_ of peak update
   */
  def modifyPeaks(peaks: NonEmptyList[PeakCone]): RVar[NonEmptyList[PeakCone]] =
    peaks.traverse(x => x.update(x))

  // Lenses for PeakCone
  val _height: Lens[PeakCone, Double] =
    Lens[PeakCone, Double](_.height)(a => b => b.copy(height = a))
  val _width: Lens[PeakCone, Double] = Lens[PeakCone, Double](_.width)(a => b => b.copy(width = a))

  val _peakGrowth: Lens[PeakCone, PeakGrowth] =
    Lens[PeakCone, PeakGrowth](_.config.growth)(a =>
      b => b.copy(config = b.config.copy(growth = a)))

  def peakEval(peaks: NonEmptyList[PeakCone]): Eval[NonEmptyList, Double] = // Should we consider multiple peak types?
    Eval.unconstrained((a: NonEmptyList[Double]) =>
      peaks.head.config.rotation match {
        case None =>
          val x = peaks.map(_.eval(a)).maximum1
          if (x == Double.NaN) Infeasible(0.0) else Feasible(x)

        case Some(Rotation(angle, (axisA, axisB))) =>
          val domain: NonEmptyList[Double] = peaks.head.domain.map(center(_))
          val centered: List[Double] = a.zip(domain).map(t => t._1 - t._2).toList

          // _Cannot_ assume that the lookup of the index will succeed
          val dimX = centered.lift(axisA)
          val dimY = centered.lift(axisB)

          val relocated: Option[NonEmptyList[Double]] =
            (dimX |@| dimY) { (a, b) =>
              val newA = Math.cos(angle) * a + Math.sin(angle) * -b
              val newB = Math.sin(angle) * a + Math.cos(angle) * b

              val centerRot_x: List[Double] =
                centered.zipWithIndex.map(
                  t =>
                    if (t._2 == axisA) newA
                    else if (t._2 == axisB) newB
                    else t._1)

              centerRot_x.zip(centered).map(x => x._1 + x._2)
            }.flatMap(_.toNel)

          relocated match {
            case None => sys.error("Error rotating input X")
            case Some(x) =>
              val fx = peaks.map(_.eval(x)).maximum1
              if (fx == Double.NaN) Infeasible(0.0) else Feasible(fx)
          }
    })
}
