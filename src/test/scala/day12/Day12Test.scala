package day12

import day12.Day12._
import org.scalatest.funsuite.AnyFunSuite

class Day12Test extends AnyFunSuite {
  test("test simulate") {
    val moons = Array(Moon(-1, 0, 2) ,Moon(2, -10, -7) ,Moon(4, -8, 8) ,Moon(3, 5, -1))
    val (updatedMoons, _) = simulate(moons, (_, steps) => steps == 10)
    assert(updatedMoons.contains(Moon(3, -6,  1, 3,  2, -3)))
  }

  test("test totalEnergy") {
    val moons = Array(Moon(-1, 0, 2) ,Moon(2, -10, -7) ,Moon(4, -8, 8) ,Moon(3, 5, -1))
    val (updatedMoons, _) = simulate(moons, (_, steps) => steps == 10)
    assert(totalEnergy(updatedMoons) == 179)
  }

  test("test universe repeating") {
    val moons = Array(Moon(-8, -10, 0), Moon(5, 5, 10), Moon(2, -7, 3), Moon(9, -8, -3))
    val (_, steps) = simulate(moons, (m, s) => m.toSet == moons.toSet && s != 0)
    assert(steps == BigInt("4686774924"))
  }
}
