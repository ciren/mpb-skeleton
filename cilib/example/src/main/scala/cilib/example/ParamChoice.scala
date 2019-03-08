package cilib
package example

object ParamChoice {

  val wList = (-0.1 to 1.0 by 0.1).toList.take(10)
  val c1List = (0.0 to 4.0 by 0.5).toList.take(10)
  val c2List = (0.0 to 4.0 by 0.5).toList.take(10)

  def main(args: Array[String]): Unit = {
    val combo =
      for {
        w <- wList
        c1 <- c1List
        c2 <- c2List
        if w != 0.0 && c1 != 0.0 && c2 != 0.0
        if (c1 + c2) < ((24 * (1 - w * w)) / (7 - 5 * w))
      } yield (w, c1, c2)

    combo.foreach(println)

    println(combo.size)
  }
}
