package day11

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

import day5.Day5._

import scala.collection.mutable

object Day11 {

  def printArea(area: mutable.Map[(Int, Int), Int]): Unit = {
    val xMax = area.keySet.map(_._1).max
    val xMin = area.keySet.map(_._1).min
    val yMax = area.keySet.map(_._2).max
    val yMin = area.keySet.map(_._2).min
    for (y <- (yMin to yMax).reverse) {
      for (x <- xMin to xMax) if (area((x, y)) == 0) print("◼️") else print("◻️")
      println()
    }
  }

  def paint(intCode: Array[BigInt], area: mutable.Map[(Int, Int), Int]): mutable.Map[(Int, Int), Int] = {
    var actionCounter = 1
    var coordinates = (0, 0)
    var direction = (0, 1)

    val input = new LinkedBlockingQueue[BigInt] {
      override def poll(timeout: Long, unit: TimeUnit): BigInt = area(coordinates)
    }

    val output = new LinkedBlockingQueue[BigInt] {
      override def add(e: BigInt): Boolean = {
        if (actionCounter % 2 == 0) {
          e.toInt match {
            case 0 => direction = (-direction._2, direction._1)
            case 1 => direction = (direction._2, -direction._1)
          }
          coordinates = (coordinates._1 + direction._1, coordinates._2 + direction._2)
        }
        else area(coordinates) = e.toInt

        actionCounter += 1
        super.add(e)
      }
    }

    compute(input, output)(State(intCode))
    area
  }

  def main(args: Array[String]): Unit = {
    val intCode = readIntCode("day11.txt")

    val areaStartingBlack = mutable.Map[(Int, Int), Int]().withDefaultValue(0)
    println(s"panels painted at least once: ${paint(intCode, areaStartingBlack).size}")

    val areaStartingWhite = mutable.Map[(Int, Int), Int]().withDefaultValue(0)
    areaStartingWhite((0, 0)) = 1
    printArea(paint(intCode, areaStartingWhite))
  }
}
