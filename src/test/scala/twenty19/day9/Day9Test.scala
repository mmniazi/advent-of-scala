package twenty19.day9

import common.IntCodeComputer._
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day9Test extends AnyFunSuite {
  test("test quine function") {
    val intCode: Array[BigInt] = Array(109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99).map(BigInt(_))
    val result = compute(input = BigInt(1) :: Nil)(State(intCode))
    assert(result == intCode.toList)
  }

  test("test large number") {
    val intCode: Array[BigInt] = Array("104", "1125899906842624", "99").map(BigInt(_))
    val result = compute(input = BigInt(1) :: Nil)(State(intCode))
    assert(result.last == BigInt("1125899906842624"))
  }

  test("test day 9 no error") {
    val source = Source.fromResource("day9.txt")
    val intCode: Array[BigInt] = source.getLines().toList.head.split(',').map(BigInt(_))
    val result = compute(input = BigInt(1) :: Nil)(State(intCode))
    assert(result.size == 1)
  }
}
