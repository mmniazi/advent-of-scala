package day16

import day16.Day16._
import org.scalatest.funsuite.AnyFunSuite


class Day16Test extends AnyFunSuite {
  test("test nthPhase small") {
    val input = List(1, 2, 3, 4, 5, 6, 7, 8)
    assert(toOutput(nthPhase(input, 4)) == "01029498")
  }

  test("test nthPhase") {
    val input = List(1, 9, 6, 1, 7, 8, 0, 4, 2, 0, 7, 2, 0, 2, 2, 0, 9, 1, 4, 4, 9, 1, 6, 0, 4, 4, 1, 8, 9, 9, 1, 7)
    assert(toOutput(nthPhase(input, 100)) == "73745418")
  }
}
