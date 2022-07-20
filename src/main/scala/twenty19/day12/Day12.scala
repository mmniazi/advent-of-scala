package twenty19.day12

import scala.annotation.tailrec

object Day12 {

  case class Moon(coord: Array[Int], vel: Array[Int] = Array(0, 0, 0))

  def calcMoonGroups(moons: Array[Moon]): Array[(Moon, Array[Moon])] = moons.map(m => (m, moons.filterNot(_ == m)))

  @tailrec
  def gcd(a: Long, b: Long): Long = if (b == 0) a.abs else gcd(b, a % b)

  def lcm(a: Long, b: Long): Long = (a * b).abs / gcd(a, b)

  def step(moonGroups: Array[(Moon, Array[Moon])], d: Int): Unit = {
    moonGroups.foreach { case (moon, otherMoons) =>
      otherMoons.foreach { otherMoon =>
        moon.vel(d) += otherMoon.coord(d).compare(moon.coord(d))
      }
    }
    moonGroups.foreach { case (moon, _) =>
      moon.coord(d) += moon.vel(d)
    }
  }

  def simulate(moons: Array[Moon], steps: Long): Array[Moon] = {
    val moonGroups = calcMoonGroups(moons)
    for (dimension <- 0 to 2) {
      for (_ <- 0L until steps) {
        step(moonGroups, dimension)
      }
    }
    moons
  }

  def notRepeated(moons: Array[Moon], moonsOriginal: Array[Moon], d: Int, steps: Long): Boolean = {
    !(moons.map(_.coord(d)).sameElements(moonsOriginal.map(_.coord(d))) && moons.map(_.vel(d)).sameElements(moonsOriginal.map(_.vel(d)))) || steps == 0
  }

  def repeatsAfter(moons: Array[Moon], moonsOriginal: Array[Moon], dimension: Int): Long = {
    val moonGroups: Array[(Moon, Array[Moon])] = calcMoonGroups(moons)
    var steps = 0L
    while (notRepeated(moons, moonsOriginal, dimension, steps)) {
      step(moonGroups, dimension)
      steps += 1
    }
    steps
  }

  def stepsBeforeRepeat(moons: Array[Moon], moonsOriginal: Array[Moon]): Long =
    (0 to 2).map(repeatsAfter(moons, moonsOriginal, _)).reduce(lcm)

  def totalEnergy(moons: Array[Moon]): Int =
    moons.map(m => m.coord.map(math.abs).sum * m.vel.map(math.abs).sum).sum

  def main(args: Array[String]): Unit = {
    def moons: Array[Moon] = Array(Moon(Array(-3, 15, -11)), Moon(Array(3, 13, -19)), Moon(Array(-13, 18, -2)), Moon(Array(6, 0, -1)))
    println(s"total energy: ${totalEnergy(simulate(moons, 1000))}")
    println(s"time before moons come to initial state: ${stepsBeforeRepeat(moons, moons)}")
  }
}