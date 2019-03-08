package cilib

import scalaz.{Ordering => _, _}
import Scalaz._

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive

object Selection {

  private implicit class RicherEphemeralStream[A](val s: EphemeralStream[A]) extends AnyVal {
    def drop(n: Int): EphemeralStream[A] = {
      @annotation.tailrec
      def go(count: Int, c: Option[EphemeralStream[A]]): EphemeralStream[A] =
        if (count > 0) go(count - 1, c.flatMap(_.tailOption))
        else c.cata(x => x, EphemeralStream())

      go(n, Option(s))
    }
  }

  def indexNeighbours[A](n: Int): (NonEmptyList[A], A) => List[A] =
    (l: NonEmptyList[A], x: A) => {
      val list = l
      val size = l.size
      val point =
        l.list.indexWhere(_ == x) match {
          case None    => 0
          case Some(i) => (i - (n / 2) + size) % size
        }
      lazy val c: EphemeralStream[A] = EphemeralStream(list.toList: _*) ++ c

      c.drop(point).take(n).toList
    }

  def latticeNeighbours[A: scalaz.Equal]: (NonEmptyList[A], A) => List[A] =
    (l: NonEmptyList[A], x: A) => {
      val list = l.list
      val np = list.length
      val index: Option[Int] = list.indexOf(x) // This returns Option[Int] instead of Int, which is awesome :)
      val sqSide = math.round(math.sqrt(np.toDouble)).toInt
      val nRows = math.ceil(np / sqSide.toDouble).toInt
      val row: Option[Int] = index.map(_ / sqSide)
      val col: Option[Int] = index.map(_ % sqSide)

      @inline def indexInto(r: Int, c: Int) =
        r * sqSide + c

      @inline val colsInRow =
        (r: Int) => if (r == nRows - 1) np - r * sqSide else sqSide

      val result = for {
        r <- row
        c <- col
        north <- list.index(
          indexInto((r - 1 + nRows) % nRows - (if (c >= colsInRow(r - 1 + nRows) % nRows) 1 else 0),
                    c))
//        _ =       println(indexInto((r - 1 + nRows) % nRows - (if (c >= colsInRow(r - 1 + nRows) % nRows) 1 else 0), c))
        south <- list.index(indexInto(if (c >= colsInRow(r + 1) % nRows) 0 else (r + 1) % nRows, c))
        //      _ = println(indexInto(if (c >= colsInRow(r + 1) % nRows) 0 else (r + 1) % nRows, c))
        east <- list.index(indexInto(r, (c + 1) % colsInRow(r)))
        //    _ = println(indexInto(r, (c + 1) % colsInRow(r)))
        west <- list.index(indexInto(r, (c - 1 + colsInRow(r)) % colsInRow(r)))
        //  _ = println(indexInto(r, (c - 1 + colsInRow(r)) % colsInRow(r)))
      } yield List(x, north, south, east, west)

      result.getOrElse(sys.error("error in latticeNeighbours - unable to determine neighbours"))
    }

  def distanceNeighbours[F[_]: Foldable, A: Order](distance: MetricSpace[F[A], A])(
      n: Int): (NonEmptyList[F[A]], F[A]) => List[F[A]] =
    (l: NonEmptyList[F[A]], x: F[A]) => l.sortBy(li => distance.dist(li, x)).toList.take(n)

  def wheel[A]: (NonEmptyList[A], A) => List[A] =
    (l: NonEmptyList[A], a: A) => {
      if (l.head == a) l.toList
      else List(l.head, a)
    }

  def star[A]: (NonEmptyList[A], A) => List[A] =
    (l: NonEmptyList[A], x: A) => l.toList

  def tournament[F[_], A](n: Int Refined Positive, l: NonEmptyList[F[A]])(
      implicit F: Fitness[F, A, A]): Comparison => RVar[Option[F[A]]] =
    o =>
      RVar
        .sample(n, l)
        .map(_.flatMap(_.reduceLeftOption((a, c) => o.apply(a, c))))

  def fitnessProportion[F[_], A](xs: NonEmptyList[F[A]])(
      implicit F: Fitness[F, A, A]): Comparison => NonEmptyList[(F[A], Double)] =
    o => {
      def fitnessOrElse(x: Option[Fit], default: => Double) =
        x.map(_.fold(penalty = _.adjust, valid = _.v, infeasible = _ => default)).getOrElse(default)

      def c(x: F[A]): Option[Fit] =
        F.fitness(x) match {
          case Some(obj) =>
            obj.fitness match {
              case -\/(a) => Some(a)
              case _      => None
            }
          case _ => None
        }

      val min: Double =
        fitnessOrElse(c(xs.foldLeft1((a, b) => if (o.apply(a, b) == a) b else a)), 1e12)
      val max: Double = fitnessOrElse(c(xs.foldLeft1((a, b) => o.apply(a, b))), -1e12)

      def scaledFitness(x: F[A]): Double = {
        val r = o.opt match {
          case Min => 1.0 / (1.0 + fitnessOrElse(c(x), min) - min)
          case Max => 1.0 / (1.0 + max - fitnessOrElse(c(x), max))
        }
//        println(s"scaled fitness: $r")
        r
      }

      val scaled: NonEmptyList[(F[A], Double)] = xs.map(x => (x, scaledFitness(x)))
      val sum = scaled.foldLeft(0.0)((a: Double, b: (F[A], Double)) => a + b._2)

      scaled.map(x => (x._1, x._2 / sum))
    }

  def rouletteWheel[F[_], A](xs: NonEmptyList[F[A]])(
      implicit F: Fitness[F, A, A]): Comparison => RVar[F[A]] =
    o => {
      val proportioned = fitnessProportion(xs).apply(o)

      Dist.stdUniform.map(r => {
        def go(sum: Double, current: F[A], remaining: List[(F[A], Double)]): F[A] =
          if (sum >= r) current
          else
            remaining match {
              case head :: tail => go(sum + head._2, head._1, tail)
              case Nil          => current
            }

        go(proportioned.head._2, proportioned.head._1, proportioned.tail.toList)
      })
    }

  def boltzmann: Double = ???

}
