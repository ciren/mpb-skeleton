package cilib
package example //import scalaz._
//import Scalaz._

object Paper1Processing {
  /*  import org.apache.commons.math3.stat.inference._
  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql.Row

  val algs = List("constRadius5", "constRadius10", "constRadius50", "gaussianRadius")
  val probs = List("mpb-static", "mpb-progressive", "mpb-abrupt", "mpb-chaotic")
  val muTest = new MannWhitneyUTest

  def main(args: Array[String]): Unit = {

    val spark = SparkSession
      .builder()
      .appName("Spark SQL basic example")
      .config("spark.master", "local")
      .getOrCreate()

    // Set the logging
    spark.sparkContext.setLogLevel("ERROR")

    // val dataFrame =
    //   spark.read.format("csv")
    //     .option("header", "true")
    //     .option("inferSchema", "true")
    //     .load("./data.csv")

    // Read the parquest file
    val dataFrame =
      spark.read.parquet("./data-test.parquet")

    println("dataFrame schema: " + dataFrame.schema)

    dataFrame.createOrReplaceTempView("d")

    //val distinctRNG = spark.sql("select * from d")
    // val nonStatic =
    //   spark.sql("""select original.alg, original.prob, original.seed, avg(offlineError)
    //        from d
    //        where offlineError is not null
    //        group by original.seed, original.alg, original.prob
    //        order by original.alg, original.prob, original.seed""")

    cme(spark)
    bebc(spark)
    beac(spark)
    //    plotRadius(spark)

  }


  def cme(spark: org.apache.spark.sql.SparkSession) = {
    val cme =
      spark.sql("""select original.alg, original.prob, original.seed, avg(original.error)
           from d
           group by original.seed, original.alg, original.prob
           order by original.alg, original.prob, original.seed""")

    //    distinctRNG.collect.foreach(println)

    val cmeMap =
      cme.rdd.map {
        case r @ Row(alg: String, prob: String, seed: Long, avg: Double) =>
          (alg, prob, seed) -> avg
      }.collectAsMap

    val cmeY: (String, String) ==>> List[Double] =
      cmeMap.foldLeft(==>>.empty[(String, String), List[Double]]) {
        case (acc, ((alg, prob, seed), avg)) =>
          acc.insertWith(
            (newValue: List[Double], existing: List[Double]) => existing ++ newValue,
            (alg, prob),
            List(avg)
          )
      }

    probs
      .foreach(prob => {
        def testPairs(l: List[String], acc: List[(String, String)]): List[(String, String)] =
          if (l.length < 2) acc
          else {
            val tp = l.tail.map(t => (l.head, t))
            testPairs(l.drop(1), acc ++ tp)
          }

        val pairs = testPairs(algs, List.empty)

        pairs.foreach {
          case (a, b) =>
            for {
              algA <- cmeY.lookup((a, prob))
              algB <- cmeY.lookup((b, prob))
            } yield {
              val p_value = muTest.mannWhitneyUTest(algA.toArray, algB.toArray)

              val winLoss = algA.zip(algB).map(x => if (x._1 < x._2) (1, 0) else (0, 1))
              val (winsA, winsB) = winLoss.foldLeft((0, 0))((a, c) => (a._1 + c._1, a._2 + c._2))

              println(
                s"CME: $prob with alg $a vs $b resulted in p-value: $p_value. Win/loss: ${winsA}/${winsB}")
            }
        }
      })

    stats("cme", cmeY)
  }

  def bebc(spark: org.apache.spark.sql.SparkSession) = {
    val bebc =
      spark.sql("""select original.alg, original.prob, original.seed, avg(bestErrorBeforeChange)
                   from d
                   where bestErrorBeforeChange is not null
                   group by original.alg, original.prob, original.seed
                   order by original.alg, original.prob, original.seed""")

    val bebcMap =
      bebc.rdd.map {
        case r @ Row(alg: String, prob: String, seed: Long, avg: Double) =>
          (alg, prob, seed) -> avg
      }.collectAsMap

    val bebcY: (String, String) ==>> List[Double] =
      bebcMap.foldLeft(==>>.empty[(String, String), List[Double]]) {
        case (acc, ((alg, prob, seed), avg)) =>
          acc.insertWith(
            (newValue: List[Double], existing: List[Double]) => existing ++ newValue,
            (alg, prob),
            List(avg)
          )
      }

    probs
      .foreach(prob => {
        def testPairs(l: List[String], acc: List[(String, String)]): List[(String, String)] =
          if (l.length < 2) acc
          else {
            val tp = l.tail.map(t => (l.head, t))
            testPairs(l.tail, acc ++ tp)
          }

        val pairs = testPairs(algs, List.empty)

        pairs.foreach {
          case (a, b) =>
            for {
              algA <- bebcY.lookup((a, prob))
              algB <- bebcY.lookup((b, prob))
            } yield {
              val p_value = muTest.mannWhitneyUTest(algA.toArray, algB.toArray)

              val winLoss = algA.zip(algB).map(x => if (x._1 < x._2) (1, 0) else (0, 1))
              val (winsA, winsB) = winLoss.foldLeft((0, 0))((a, c) => (a._1 + c._1, a._2 + c._2))

              println(
                s"ABEBC: $prob with alg $a vs $b resulted in p-value: $p_value. Win/loss: ${winsA}/${winsB}")
            }
        }
      })

    stats("bebc", bebcY)
  }

  def beac(spark: org.apache.spark.sql.SparkSession) = {
        val beac =
      spark.sql("""select original.alg, original.prob, original.seed, avg(bestErrorAfterChange)
                   from d
                   where bestErrorAfterChange is not null
                   group by original.alg, original.prob, original.seed
                   order by original.alg, original.prob, original.seed""")

    val beacMap =
      beac.rdd.map {
        case r @ Row(alg: String, prob: String, seed: Long, avg: Double) =>
          (alg, prob, seed) -> avg
      }.collectAsMap

    val beacY: (String, String) ==>> List[Double] =
      beacMap.foldLeft(==>>.empty[(String, String), List[Double]]) {
        case (acc, ((alg, prob, seed), avg)) =>
          acc.insertWith(
            (newValue: List[Double], existing: List[Double]) => existing ++ newValue,
            (alg, prob),
            List(avg)
          )
      }

    probs
      .foreach(prob => {
        def testPairs(l: List[String], acc: List[(String, String)]): List[(String, String)] =
          if (l.length < 2) acc
          else {
            val tp = l.tail.map(t => (l.head, t))
            testPairs(l.drop(1), acc ++ tp)
          }

        val pairs = testPairs(algs, List.empty)

        pairs.foreach {
          case (a, b) =>
            for {
              algA <- beacY.lookup((a, prob))
              algB <- beacY.lookup((b, prob))
            } yield {
              val p_value = muTest.mannWhitneyUTest(algA.toArray, algB.toArray)

              val winLoss = algA.zip(algB).map(x => if (x._1 < x._2) (1, 0) else (0, 1))
              val (winsA, winsB) = winLoss.foldLeft((0, 0))((a, c) => (a._1 + c._1, a._2 + c._2))

              println(
                s"ABEAC: $prob with alg $a vs $b resulted in p-value: $p_value. Win/loss: ${winsA}/${winsB}")
            }
        }
      })


    stats("beac", beacY)
  }

  def plotRadius(spark: org.apache.spark.sql.SparkSession) = {
    val radius =
      spark.sql("""select original.alg, original.prob, original.iteration, avg(original.data[1])
                   from d
                   group by original.alg, original.prob, original.iteration
                   order by original.alg, original.prob, original.iteration""")

    // val beacMap =
    //   beac.rdd.map {
    //     case r@Row(alg: String, prob: String, seed: Long, avg: Double) =>
    //       (alg, prob, seed) -> avg
    //   }.collectAsMap

    val output = new java.io.PrintWriter(new java.io.FileWriter("plot-data.csv"), true)
    radius.collect.foreach(output.println)
    output.close()
  }

  def stats(name: String, m: (String, String) ==>> List[Double]) = {
    val output = new java.io.PrintWriter(new java.io.FileWriter(name + ".data"), true)
    for {
      p <- probs
      a <- algs
    } yield {
      m.lookup((a, p))
        .map(_.toArray)
        .foreach(list => {
          val stats = new org.apache.commons.math3.stat.descriptive.DescriptiveStatistics(list)
          val data = s"${stats.getMin},${stats.getPercentile(25)},${stats.getPercentile(50)},${stats.getPercentile(75)},${stats.getMax}"
          println(data)
          output.println(data)
          // println(stats.toString)
          // println(stats.getPercentile(25))
          // println(stats.getPercentile(75))
        })
    }
    output.close()
  }
 */
}
