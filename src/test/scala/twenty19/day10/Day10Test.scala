package twenty19.day10

import twenty19.day10.Day10._
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

  test("test coordinatesForNthToVaporize") {
    val lines =
      """.#..##.###...#######
        |##.############..##.
        |.#.######.########.#
        |.###.#######.####.#.
        |#####.##.#.##.###.##
        |..#####..#.#########
        |####################
        |#.####....###.#.#.##
        |##.#################
        |#####.##.###..####..
        |..######..##.#######
        |####.##.####...##..#
        |.#####..#.######.###
        |##...#.##########...
        |#.##########.#######
        |.####.#.###.###.#.##
        |....##.##.###..#####
        |.#.#.###########.###
        |#.#.#.#####.####.###
        |###.##.####.##.#..##""".stripMargin.split('\n').toList

    val asteroids = takeAsteroids(lines)
    val (station, _) = asteroidWithMaxVisibility(asteroids)
    val result = coordinatesForNthToVaporize(asteroids, station, 200)

    assert(result == (8, 2))
  }
}
