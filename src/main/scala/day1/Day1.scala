package day1

import scala.annotation.tailrec
import scala.io.Source

object Day1 {
  def fuelReq(mass: Double): Double = math.floor(mass / 3) - 2

  @tailrec
  def fuelReqCorrected(mass: Double, accumulator: Double = 0): Double = {
    val fuel = fuelReq(mass)
    if (fuel <= 0) accumulator
    else fuelReqCorrected(fuel, accumulator + fuel)
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("day1.txt")

    val componentsMass = source
      .getLines()
      .map(_.toDouble)
      .toList

    println(s"part 1 solution: ${componentsMass.map(fuelReq).sum}")
    println(s"part 2 solution: ${componentsMass.map(fuelReqCorrected(_)).sum}")
  }
}
