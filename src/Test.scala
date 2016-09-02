import org.apache.spark
import scala.math.random
import org.apache.spark.{SparkConf, SparkContext}

/**
  * Created by zhuting on 16/8/20.
  */
object Test {
  def main(args: Array[String]) {
    val conf = new SparkConf().setAppName("TSP")
    val spark = new SparkContext(conf)

    val cities = new Cities()
    cities.loadData("/Users/zhuting/Downloads/att48_d_10628.txt")
    val manager = new PopulationManager(cities, spark, timeLimit = 100000)
    manager.evolve()

//    val slices = if (args.length > 0) args(0).toInt else 2
//    val n = math.min(100000L * slices, Int.MaxValue).toInt // avoid overflow
//    val count = spark.parallelize(1 until n, slices).map { i =>
//      val x = random * 2 - 1
//      val y = random * 2 - 1
//      if (x*x + y*y < 1) 1 else 0
//    }.reduce(_ + _)
//    println("Pi is roughly " + 4.0 * count / n)
    spark.stop()
  }
}
