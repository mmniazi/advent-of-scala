package twenty19.day5

import common.IntCodeComputer._

object Day5 {
  def main(args: Array[String]): Unit = {
    val intCode = readIntCode("day5.txt")
    val firstPartOutput = compute(input = BigInt(1) :: Nil)(State(intCode))
    println(s"part1 diagnostic code: ${firstPartOutput.last}")
    val secondPartOutput = compute(input = BigInt(5) :: Nil)(State(intCode))
    println(s"part2 diagnostic code: ${secondPartOutput.last}")
  }
}
