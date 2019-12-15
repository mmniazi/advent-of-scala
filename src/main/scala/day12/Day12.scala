package day12

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._

object Day12 {

  case class Moon(x: Int, y: Int, z: Int, vX: Int = 0, vY: Int = 0, vZ: Int = 0)

  @tailrec
  def simulate(moons: Array[Moon], stopCondition: (Array[Moon], BigInt) => Boolean, steps: BigInt = 0): (Array[Moon], BigInt) = {
    if (stopCondition(moons, steps)) (moons, steps)
    else {
      val moonPairs = moons.combinations(2).flatMap(_.permutations).map(m => (m(0), m(1))).toList.groupBy(_._1).par
      val updatedMoons = moonPairs.map { case (moon, gravityPair) =>
        val velocityDelta = gravityPair.map { case (moon1, moon2) =>
          Seq(moon2.x.compare(moon1.x), moon2.y.compare(moon1.y), moon2.z.compare(moon1.z))
        }.transpose.map(_.sum)
        val vX = moon.vX + velocityDelta.head
        val vY = moon.vY + velocityDelta(1)
        val vZ = moon.vZ + velocityDelta(2)
        Moon(moon.x + vX, moon.y + vY, moon.z + vZ, vX, vY, vZ)
      }.toArray

      simulate(updatedMoons, stopCondition, steps + 1)
    }
  }

  def totalEnergy(moons: Array[Moon]): Int = moons.map(moon =>
    (math.abs(moon.x) + math.abs(moon.y) + math.abs(moon.z)) * (math.abs(moon.vX) + math.abs(moon.vY) + math.abs(moon.vZ))
  ).sum

  def main(args: Array[String]): Unit = {
    val moons = Array(Moon(-3, 15, -11), Moon(3, 13, -19), Moon(-13, 18, -2), Moon(6, 0, -1))
    val (updatedMoons, _) = simulate(moons, (_, steps) => steps == 1000)
    println(s"total energy: ${totalEnergy(updatedMoons)}")
  }
}
