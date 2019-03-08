package cilib
package exec

import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._

sealed abstract class Env
final case object Unchanged extends Env
final case object Change extends Env

object Env {
  def unchanging: Stream[Env] =
    Stream(Unchanged) #::: unchanging

  def frequency[A](n: Int Refined Positive): Stream[Env] =
    (unchanging.take(n - 1) #::: Stream[Env](Change)) #::: frequency(n)
}
