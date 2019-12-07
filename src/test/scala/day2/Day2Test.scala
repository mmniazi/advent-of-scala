package day2

import day2.Day2._
import org.scalatest.funsuite.AnyFunSuite

class Day2Test extends AnyFunSuite {

  test("test compute simple") {
    val result = compute(Array(1, 0, 0, 0, 99))
    val expected = Array(2, 0, 0, 0, 99)
    assert(result sameElements expected)
  }

  test("test compute complex") {
    val result = compute(
      Array(
        1, 9, 10, 3,
        2, 3, 11, 0,
        99,
        30, 40, 50
      )
    )

    val expected = Array(
      3500, 9, 10, 70,
      2, 3, 11, 0,
      99,
      30, 40, 50
    )

    assert(result sameElements expected)
  }
}
