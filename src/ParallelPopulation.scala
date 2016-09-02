import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

/**
  * Created by zhuting on 16/8/20.
  */
class ParallelPopulation(override val cities: Cities, val sc : SparkContext, val slices : Int = 2) extends Population(cities) {
  genOriginal()
  var individualsRDD = sc.parallelize(individuals, slices)
  var currentBest = individuals(0)
//  var individualProbRDD

  override def calFitness(): Unit = {
     individualsRDD = individualsRDD.map(i => {
      i.calFitness()
      i
    })
    currentBest = individualsRDD.reduce((i1, i2) => {
      if (i1.fitness > i2.fitness) i1
      else i2
    })
  }

  override def calProbability(): Unit = {
    var distSum = individualsRDD.map(i => i.distanceSum).reduce(_ + _)
    individualsRDD = individualsRDD.map(i => {
      i.fitness = 1 - i.distanceSum.toDouble / distSum.toDouble
      i
    })

    var fitnessSum = individualsRDD.map(i => i.fitness).reduce(_ + _)
//    var individualsPRDD = individualsRDD.map(i => (i, i.fitness / fitnessSum)).co

     //(individual, prob)
  }
}
