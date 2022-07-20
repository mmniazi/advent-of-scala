package twenty19.day18

import org.scalatest.funsuite.AnyFunSuite

import twenty19.day18.Day18._

class Day18Test extends AnyFunSuite {
  test("findPath") {
    val inputMap = readMap(
      """########################
        |#f.D.E.e.C.b.A.@.a.B.c.#
        |######################.#
        |#d.....................#
        |########################""".stripMargin.split('\n').toList)

    val start = startCoord(inputMap)

    println(findPaths(start, inputMap, start :: Nil))
  }
}
