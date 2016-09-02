import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by zhuting on 16/8/18.
  */
class Population(val cities: Cities, val size:Int = 100, val timeLimit:Int = Int.MaxValue, val evolveLimit:Int = 100, val mutateRate: Double = 0.4, val crossoverRate:Double = 0.8) extends java.io.Serializable {
  var individuals = new Array[Individual](size)
  var bestFitness = 0.0
  var minDistance = Int.MaxValue
  var bestIndividual = -1
  var probability = new Array[Double](size)
  var evolveCnt = 0
  var startTime = System.currentTimeMillis / 1000


  def randomArray(n : Int) = {
    var arr = 0 until n toArray
    var outList:List[Int] = Nil
    var border = n
    for (i <- 0 until n){
      val index=(new Random).nextInt(border)
      outList = outList:::List(arr(index))
      arr(index) = arr.last
      arr = arr.dropRight(1)
      border -= 1
    }
    outList.toArray
  }

  def genOriginal(): Unit ={
    //    individuals.foreach(i => i = new Individual(randomArray(cities.cityNum), cities))
    for (i <- 0 until size){
      individuals(i) = new Individual(randomArray(cities.cityNum), cities)
    }
  }

  def calFitness(): Unit ={
    individuals.foreach(i => i.calFitness())
    for (i <- 0 until individuals.length){
      if (minDistance > individuals(i).distanceSum){
        minDistance = individuals(i).distanceSum
        bestFitness = individuals(i).fitness
        bestIndividual = i
      }
    }
  }

  def calProbability(): Unit ={
    //adjust fitness
    var distSum = 0
    individuals.foreach(i => distSum += i.distanceSum)
    for (i <- 0 until size){
      individuals(i).fitness = 1.0 - individuals(i).distanceSum.toDouble / distSum.toDouble
    }
    //if (evolveCnt % 100 == 0) println(probability(bestIndividual))

    var fitnessSum = 0.0
    individuals.foreach(i => fitnessSum += i.fitness)
    for (i <- 0 until size){
      probability(i) = individuals(i).fitness / fitnessSum
    }
    //    if (evolveCnt % 100 == 0) println(probability(bestIndividual))
  }

  def roundRobinSelect() = {
    //    if(probability(bestIndividual) > 0.5)
    //      (new Random).nextInt(size)
    //    else {
    val targetProbabilitySum = (new Random()).nextDouble()
    var select = size - 1
    var curProbabilitySum = 0.0
    var found = false
    for (i <- 0 until size if found == false) {
      curProbabilitySum += probability(i)
      if (curProbabilitySum > targetProbabilitySum) {
        select = i
        found = true
      }
    }
    //    println(select)
    select
    //    }
  }

  def crossover(n:Int) ={
    var newIndividuals = new Array[Individual](n)
    calProbability()
    for(i <- 0 until n){
      newIndividuals(i) = if ((new Random).nextDouble() <= crossoverRate && i != bestIndividual)
        Individual.crossover(individuals(roundRobinSelect()), individuals(roundRobinSelect()))
      else
        individuals(roundRobinSelect())
    }
    //    newIndividuals.map(i => Individual.crossover(individuals(roundRobinSelect()), individuals(roundRobinSelect())))
    newIndividuals
  }

  def mutate(): Int ={
    var mutateNum = 0
    for (i <- 1 until size){
      if ((new Random()).nextDouble() <= mutateRate){
        mutateNum += 1
        var tmp = individuals(i).mutate()
        individuals(i) = tmp
      }
    }
    if ((new Random()).nextDouble() <= mutateRate){
      var tmp = individuals(0).mutate()
      tmp.calFitness()
      if(individuals(0).distanceSum > tmp.distanceSum)
        individuals(0) = tmp
    }

    mutateNum
  }

  //TODO error exists here  //FIXED
  def genNextGeneration(): Unit ={
    var tmp = individuals(bestIndividual)
    //crossover
    val newChildren = crossover(size - 1)
    //    print(newChildren.length)
    for (i <- 1 until size){
      individuals(i) = newChildren(i - 1)
    }

    //keep the best individual
    individuals(0) = tmp
    bestIndividual = 0
    //mutate
    mutate()
  }

  def printInfo(): Unit ={
    print(evolveCnt + ",")
    print((System.currentTimeMillis - startTime) / 1000)
    print(",")
    println(minDistance)
  }

  def checkLimit(): Boolean ={
    //    println("checking...")
    if (System.currentTimeMillis - startTime > timeLimit) true
    else if (evolveCnt >= evolveLimit) true
    else false
  }


  def evolve(): Unit ={
    startTime = System.currentTimeMillis
    genOriginal()
    var limit = false
    var lastBest = Int.MaxValue
    while (!limit){
      calFitness()
      if(evolveCnt % 20000 == 0 || minDistance != lastBest) {printInfo(); lastBest = minDistance;}
      genNextGeneration()
      limit = checkLimit()
      evolveCnt += 1
    }
    printInfo()
  }
}