package cilib

import scalaz._, Scalaz._
import spire.math.Numeric

object Diversity {

  def averageDiversity[F[_], A](x: F[Position[A]])(
      implicit F: Foldable[F],
      N: Numeric[A]): Double = { // Kinda a matrix of entiies

    val transposed: List[List[Double]] = // determine the mean column vectors
      F.toList(x).map(_.pos.map(N.toDouble).toList).transpose

    val listOfSquared: List[List[Double]] = transposed.map(x => {
      val mean = x.sum / x.length.toDouble
      x.map(y => {
        val y2 = y - mean
        y2 * y2
      })
    })

    val result = listOfSquared.map(x => math.sqrt(x.sum)).sum / x.length.toDouble

    if (result.isNaN || result.isInfinity) {
      // println(s"Found NaN.\n$x")
      0.0
    } else result
  }

}
