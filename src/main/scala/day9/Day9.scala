package day9

import day5.Day5._

object Day9 {

  def main(args: Array[String]): Unit = {
    val intCode = readIntCode("day9.txt")
    val boostKeyCode = compute(input = BigInt(1) :: Nil)(State(intCode)).head
    println(s"BOOST key code: $boostKeyCode")
    val distressCoordinates = compute(input = BigInt(2) :: Nil)(State(intCode)).head
    println(s"distress signal coordinates: $distressCoordinates")
  }
}
