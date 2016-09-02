import org.apache.spark.SparkContext

/**
  * Created by zhuting on 16/8/21.
  */
class PopulationManager(val cities: Cities, val sc : SparkContext, val slices : Int = 4, val timeLimit: Int = Int.MaxValue, val evolveLimit: Int = Int.MaxValue) extends java.io.Serializable{
  val populationArr = new Array[EnhancedPopulation](slices)
  for (i <- 0 until slices){
    populationArr(i) = new EnhancedPopulation
  }

  var populationRDD = sc.parallelize(populationArr, slices)
  var startTime = System.currentTimeMillis / 1000
//  var bestIndividuals = new Array[Individual](1)
  var evolveCnt = 0
  var minDistance = Int.MaxValue

  def printInfo(): Unit ={
    print(evolveCnt + ",")
    print((System.currentTimeMillis - startTime) / 1000)
    print(",")
    println(minDistance)
  }


  def checkLimit(): Boolean ={
    if (System.currentTimeMillis - startTime > timeLimit) true
    else if (evolveCnt >= evolveLimit) true
    else false
  }

  def evolve(): Unit ={
    startTime = System.currentTimeMillis
    var limit = false
    populationRDD = populationRDD.map(p => {
      p.genOriginal()
      p
    }).cache()

    minDistance = populationRDD.map(p => {
      p.calFitness()
      p
    }).reduce((p1, p2)=>{
      if (p1.minDistance < p2.minDistance) p1
      else p2
    }).minDistance
    printInfo()
    var lastBest = Int.MaxValue
    while(!limit){
      populationRDD = populationRDD.map(p => {
        p.evolveFor(1000)
        p
      }).cache()
      val bestIndividuals = populationRDD.map(p => p.individuals(0)).collect()
      minDistance = bestIndividuals.reduce((i1, i2) => if (i1.distanceSum < i2.distanceSum) i1 else i2).distanceSum
      evolveCnt += 1000
      if(evolveCnt % 5000 == 0 || minDistance != lastBest) {printInfo(); lastBest = minDistance;}

      populationRDD = populationRDD.map(p => {
        p.updateWith(bestIndividuals)
        p
      }).cache()
      limit = checkLimit()
    }
    printInfo()
  }
}
