package day3

import day3.Day3._
import org.scalatest.funsuite.AnyFunSuite

class Day3Test extends AnyFunSuite {

  test("test closestIntersectToCenter") {
    val wire1Points = computePoints("R75,D30,R83,U83,L12,D49,R71,U7,L72")
    val wire2Points = computePoints("U62,R66,U55,R34,D71,R55,D58,R83")

    assert(closestIntersectToCenter(wire1Points, wire2Points) == 159)
  }

  test("test closestIntersectByDistance") {
    val wire1Points = computePoints("R75,D30,R83,U83,L12,D49,R71,U7,L72")
    val wire2Points = computePoints("U62,R66,U55,R34,D71,R55,D58,R83")

    assert(closestIntersectByDistance(wire1Points, wire2Points) == 610)
  }
}
