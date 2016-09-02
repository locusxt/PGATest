

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by zhuting on 16/8/18.
  */
class Individual(val chromosome: Array[Int], val cities: Cities) extends java.io.Serializable{
  var distanceSum = Int.MaxValue
  var fitness = Double.MaxValue

  def calFitness() = {
    val distances = for (i <- 1 until chromosome.length) yield cities.distMatrix(chromosome(i - 1))(chromosome(i))
    distanceSum = distances.toList.+:(cities.distMatrix(chromosome(chromosome.length - 1 ))(chromosome(0))).sum
    fitness = 1 / distanceSum.toDouble
    fitness
  }

  def mutate() = {
    val i = (new Random).nextInt(chromosome.length)
    var tmp = (new Random).nextInt(chromosome.length - 1)
    val j = if (tmp >= i) tmp + 1 else tmp
    val newChromosome = this.chromosome.toBuffer

    //just a swap(i, j)
    tmp = newChromosome(j)
    newChromosome(j) = newChromosome(i)
    newChromosome(i) = tmp

    new Individual(newChromosome.toArray, this.cities)
  }
}

object Individual{
  //greedy crossover
  def crossover(p1 : Individual, p2 : Individual) = {
    val parentChromosome = Array(p1.chromosome, p2.chromosome)
    val unvisited = new scala.collection.mutable.HashMap[Int, Boolean]
    for (i <- 0 until p1.cities.cityNum) unvisited += (i -> true)

    val newChromosome = new ArrayBuffer[Int]()
    var startCity = parentChromosome((new Random).nextInt(2))(0)
    newChromosome += startCity
    unvisited -= startCity
    for (i <- 1 until p1.cities.cityNum){
      val route1 = p1.cities.distMatrix(startCity)(parentChromosome(0)(i))
      val route2 = p1.cities.distMatrix(startCity)(parentChromosome(1)(i))
      if (unvisited.getOrElse(parentChromosome(0)(i), false) && route1 < route2
        || !unvisited.getOrElse(parentChromosome(1)(i), false)){
        startCity = parentChromosome(0)(i)
        newChromosome += startCity
        unvisited -= startCity
      }
      else if (unvisited.getOrElse(parentChromosome(1)(i), false)){
        startCity = parentChromosome(1)(i)
        newChromosome += startCity
        unvisited -= startCity
      }
      else{
        val tmp = (new Random).nextInt(unvisited.keySet.size)
        startCity = unvisited.keySet.toArray.apply(tmp)
        newChromosome += startCity
        unvisited -= startCity
      }
    }
    new Individual(newChromosome.toArray, p1.cities)
  }
}

