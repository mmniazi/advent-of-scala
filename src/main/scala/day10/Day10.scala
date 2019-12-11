package day10

import scala.io.Source
import scala.math.BigDecimal.RoundingMode

object Day10 {
  private def round(d: Double): Double = BigDecimal(d).setScale(3, RoundingMode.CEILING).toDouble

  def calculateAngle(center: (Int, Int), point: (Int, Int)): Double = round(math.atan2(point._2 - center._2, point._1 - center._1))

  def takeAsteroids(lines: List[String]): List[(Int, Int)] =
    lines.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.collect { case ('#', x) => (x, y) }
    }

  def asteroidWithMaxVisibility(asteroids: List[(Int, Int)]): ((Int, Int), Int) = {
    asteroids.map(asteroid => {
      val asteroidsWithVisibilityCount = asteroids
        .filterNot(_ == asteroid)
        .map(otherAsteroid => (otherAsteroid, calculateAngle(asteroid, otherAsteroid)))
        .distinctBy(_._2)
        .length
      (asteroid, asteroidsWithVisibilityCount)
    }).maxBy(_._2)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day10.txt").getLines()
    val asteroids = takeAsteroids(lines.toList)
    println(asteroidWithMaxVisibility(asteroids))
  }
}
