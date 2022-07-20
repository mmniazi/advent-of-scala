package twenty19.day10

import scala.annotation.tailrec
import scala.io.Source
import scala.math.BigDecimal.RoundingMode

object Day10 {
  private def round(d: Double): Double = BigDecimal(d).setScale(3, RoundingMode.CEILING).toDouble

  def calculateAngle(center: (Int, Int), point: (Int, Int)): Double =
  // This rotates the whole coordinate system by 90 degrees to make laser calculation trivial
    round(math.atan2(center._1 - point._1 , point._2 - center._2))

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

  @tailrec
  def coordinatesForNthToVaporize(asteroids: List[(Int, Int)], station: (Int, Int), n: Int, asteroidsDestroyedPrev: Int = 0): (Int, Int) = {
    val visibleAsteroids = asteroids
      .filterNot(_ == station)
      .map(otherAsteroid => (otherAsteroid, calculateAngle(station, otherAsteroid)))
      .distinctBy(_._2)
      .sortBy(_._2)
      .map(_._1)

    val asteroidsDestroyedTotal = visibleAsteroids.size + asteroidsDestroyedPrev
    if (asteroidsDestroyedTotal < n)
      coordinatesForNthToVaporize(asteroids diff visibleAsteroids, station, n, asteroidsDestroyedTotal)
    else
      visibleAsteroids(n - asteroidsDestroyedPrev - 2)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day10.txt").getLines()
    val asteroids = takeAsteroids(lines.toList)
    val (station, asteroidsVisibleFromStation) = asteroidWithMaxVisibility(asteroids)
    println(s"asteroids visible from station: $asteroidsVisibleFromStation")
    println(s"position of 200th asteroid to be destroyed: ${coordinatesForNthToVaporize(asteroids, station, 200)}")
  }
}
