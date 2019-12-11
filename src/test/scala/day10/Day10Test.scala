package day10

import day10.Day10._
import org.scalatest.funsuite.AnyFunSuite

class Day10Test extends AnyFunSuite {

  test("test asteroidWithMaxVisibility") {
    val lines =
      """......#.#.
        |#..#.#....
        |..#######.
        |.#.#.###..
        |.#..#.....
        |..#....#.#
        |#..#....#.
        |.##.#..###
        |##...#..#.
        |.#....####""".stripMargin.split('\n').toList

    val asteroids = takeAsteroids(lines)
    val result = asteroidWithMaxVisibility(asteroids)
    assert(result == ((5,8), 33))
  }
}
