package twenty19.day8

import twenty19.day8.Day8._
import org.scalatest.funsuite.AnyFunSuite

class Day8Test extends AnyFunSuite {
  test("test layerWithMinZeroes") {
    val input = List(1, 2, 2, 1, 0, 1, 1, 2, 0, 0, 0, 0)
    assert(calculateHash(input, 3 * 2) == 6)
  }

  test("test decodeImage") {
    val input = List(0, 2, 2, 2, 1, 1, 2, 2, 2, 2, 1, 2, 0, 0, 0, 0)
    assert(decodeImage(input, 2 * 2) == List(0,1,1,0))
  }
}
