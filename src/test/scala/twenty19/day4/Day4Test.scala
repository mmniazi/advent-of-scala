package twenty19.day4

import twenty19.day4.Day4._
import org.scalatest.funsuite.AnyFunSuite

class Day4Test extends AnyFunSuite {

  test("test hasIncreasingDigits") {
    assert(hasIncreasingDigits("111111"))
    assert(hasIncreasingDigits("123456"))
    assert(!hasIncreasingDigits("121456"))
    assert(!hasIncreasingDigits("421456"))
  }

  test("test groupConsecutive") {
    assert(groupConsecutive("111111".toList) == List(('1', 6)))
    assert(groupConsecutive("123323".toList) == List(('1', 1), ('2', 1), ('3', 2), ('2', 1), ('3', 1)))
  }
}
