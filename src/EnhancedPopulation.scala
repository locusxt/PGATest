/**
  * Created by zhuting on 16/8/21.
  */
class EnhancedPopulation extends Population(new Cities) with java.io.Serializable{
  cities.loadData("/Users/zhuting/Downloads/att48_d_10628.txt")

  override def calFitness(): Unit = {
    individuals.foreach(i => i.calFitness())
    individuals = individuals.sortWith(_.distanceSum < _.distanceSum)
    bestIndividual = 0
    minDistance = individuals(0).distanceSum
    bestFitness = individuals(0).fitness
  }

  def updateWith(arr : Array[Individual]): Unit = {
    individuals(0) = arr(0)
    for (i <- 1 until arr.length){
      individuals(size - i) = arr(i)
    }
  }

  def evolveFor(times: Int = 1): Unit ={
    for (i <- 0 until times) {
      calFitness()
      genNextGeneration()
    }
  }
}
