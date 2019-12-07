package day3

import scala.io.Source

object Day3 {
  def computePoints(wire: String): Map[(Int, Int), Int] = {
    val wireParts = wire.split(',')
    var lastCorner = (0, 0, 0)

    wireParts.flatMap { vector =>
      val direction = vector.head
      val steps = vector.tail.toInt

      val points = (1 to steps) map { step =>
        direction match {
          case 'U' => (lastCorner._1, lastCorner._2 + step, lastCorner._3 + step)
          case 'D' => (lastCorner._1, lastCorner._2 - step, lastCorner._3 + step)
          case 'R' => (lastCorner._1 + step, lastCorner._2, lastCorner._3 + step)
          case 'L' => (lastCorner._1 - step, lastCorner._2, lastCorner._3 + step)
        }
      }

      lastCorner = points.last

      points
    }.groupMapReduce {
      case (x, y, _) => (x, y) // group by coordinates of points
    } {
      case (_, _, distance) => distance // extract distance for each point
    } {
      case (distance1, distance2) => math.min(distance1, distance2) // select minimum distance for each point
    }
  }

  def closestIntersectToCenter(wire1: Map[(Int, Int), Int], wire2: Map[(Int, Int), Int]): Int =
    wire1.keySet.intersect(wire2.keySet)
      .map { case (x, y) => math.abs(x) + math.abs(y) }
      .min

  def closestIntersectByDistance(wire1: Map [(Int, Int), Int], wire2: Map[(Int, Int), Int]): Int =
    wire1.keySet.intersect(wire2.keySet)
      .map(coords => wire1(coords) + wire2(coords))
      .min


  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day3.txt").getLines().toArray
    val wire1Points = computePoints(input(0))
    val wire2Points = computePoints(input(1))

    println(closestIntersectToCenter(wire1Points, wire2Points))
    println(closestIntersectByDistance(wire1Points, wire2Points))
  }
}
