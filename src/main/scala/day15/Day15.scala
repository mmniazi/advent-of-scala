package day15

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

import common.IntCodeComputer._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Day15 {

  case class Direction(index: Int) {
    def otherDirections: Seq[Direction] = List(1, 2, 3, 4).filterNot(_ == index).map(Direction(_))

    def vector: (Int, Int) = index match {
      case 0 => (0, 0)
      case 1 => (0, 1)
      case 2 => (0, -1)
      case 3 => (-1, 0)
      case 4 => (1, 0)
    }

    def reverse: Direction = index match {
      case 0 => Direction(0)
      case 1 => Direction(2)
      case 2 => Direction(1)
      case 3 => Direction(4)
      case 4 => Direction(3)
    }
  }

  object Direction {
    val all: Seq[Direction] = Direction(0).otherDirections
  }

  case class Coord(x: Int, y: Int) {
    def +(direction: Direction): Coord = Coord(x + direction.vector._1, y + direction.vector._2)
  }

  val (unknown, wall, traversable, goal, pending) = ("?", "#", ".", "*", "%")

  def buildMap(): mutable.Map[Coord, String] = {
    val intCode = readIntCode("day15.txt")
    val map = mutable.Map[Coord, String](Coord(0, 0) -> traversable).withDefaultValue(unknown)
    val pendingMoves = mutable.Stack[Direction](Direction.all.flatMap(d => Seq(d.reverse, d)): _*)
    val input = new LinkedBlockingQueue[BigInt]()
    val output = new LinkedBlockingQueue[BigInt]()
    Future(compute(input, output)(State(intCode)))

    @tailrec
    def _buildMap(coord: Coord): mutable.Map[Coord, String] = {
      if (pendingMoves.isEmpty) return map

      val direction = pendingMoves.pop()
      val nextCoord = coord + direction
      input.add(direction.index)
      map(nextCoord) = output.poll(1, TimeUnit.DAYS).toInt match {
        case 0 => wall
        case 1 => traversable
        case 2 => goal
      }

      map(nextCoord) match {
        case `wall` =>
          pendingMoves.pop()
          _buildMap(coord)
        case `traversable` | `goal` =>
          direction.reverse.otherDirections.foreach { d =>
            if (map(nextCoord + d) == unknown) {
              map(nextCoord + d) = pending
              pendingMoves.push(d.reverse)
              pendingMoves.push(d)
            }
          }
          _buildMap(nextCoord)
      }
    }

    _buildMap(Coord(0, 0))
  }

  def createDistanceMap(map: mutable.Map[Coord, String], start: Coord): mutable.Map[Coord, Int] = {
    val distanceMap = mutable.Map[Coord, Int](start -> 0)
    val pendingCoord = mutable.Stack[(Coord, Int)]((start, 0))

    @tailrec
    def _buildMap(): mutable.Map[Coord, Int] = {
      if (pendingCoord.isEmpty) return distanceMap

      val (coord, distance) = pendingCoord.pop()
      distanceMap(coord) = distance
      if (List(`traversable`, `goal`) contains map(coord))
        pendingCoord.pushAll(
          Direction.all
            .map(dir => (coord + dir, distance + 1))
            .filter{case (c, d) => d < distanceMap.getOrElse(c, Int.MaxValue)}
        )
      _buildMap()
    }

    _buildMap()
  }

  def printMap(map: mutable.Map[Coord, String]): Unit = {
    val xRange = map.keys.map(_.x).min to map.keys.map(_.x).max
    val yRange = map.keys.map(_.y).min to map.keys.map(_.y).max
    for (y <- yRange) {
      for (x <- xRange) {
        print(map(Coord(x, y)))
      }
      println()
    }
  }

  def main(args: Array[String]): Unit = {
    val map = buildMap()
    printMap(map)
    val goalCoord = map.find(_._2 == goal).get._1
    val distanceFromStart = createDistanceMap(map, Coord(0, 0))
    val distanceFromGoal = createDistanceMap(map, goalCoord)
    println(s"Min distance to oxygen room: ${distanceFromStart(goalCoord)}")
    println(s"steps for oxygen to spread everywhere: ${distanceFromGoal.filter(t => map(t._1) == traversable).values.max}")
  }
}