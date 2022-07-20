package twenty19.day12

import twenty19.day12.Day12._
import org.scalatest.funsuite.AnyFunSuite

class Day12Test extends AnyFunSuite {
  test("test simulate") {
    val moons = Array(Moon(Array(-1, 0, 2)), Moon(Array(2, -10, -7)), Moon(Array(4, -8, 8)), Moon(Array(3, 5, -1)))
    simulate(moons, 10)
    assert(moons.exists(m => (m.coord sameElements Array(3, -6, 1)) && (m.vel sameElements Array(3, 2, -3))))
  }

  test("test totalEnergy") {
    val moons = Array(Moon(Array(-1, 0, 2)), Moon(Array(2, -10, -7)), Moon(Array(4, -8, 8)), Moon(Array(3, 5, -1)))
    simulate(moons, 10)
    assert(totalEnergy(moons) == 179)
  }

  test("test universe repeating") {
    def moons = Array(Moon(Array(-8, -10, 0)), Moon(Array(5, 5, 10)), Moon(Array(2, -7, 3)), Moon(Array(9, -8, -3)))

    assert(stepsBeforeRepeat(moons, moons) == BigInt("4686774924"))
  }
}
