package twenty19.day5

import common.IntCodeComputer._
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day5Test extends AnyFunSuite {

  test("test part1 all outputs valid") {
    val source = Source.fromResource("day5.txt")
    val intCode: Array[Int] = source.getLines().toList.head.split(',').map(_.toInt)

    val output = compute(input = BigInt(1) :: Nil)(State(intCode.map(BigInt(_))))
    assert(output.reverse.tail.forall(_ == 0))
  }

  test("test number compared to 8") {
    val intCode = Array(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
      1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
      999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99)

    assert(compute(BigInt(7) :: Nil)(State(intCode.map(BigInt(_)))).head == 999)
    assert(compute(BigInt(8) :: Nil)(State(intCode.map(BigInt(_)))).head == 1000)
    assert(compute(BigInt(9) :: Nil)(State(intCode.map(BigInt(_)))).head == 1001)
  }
}
