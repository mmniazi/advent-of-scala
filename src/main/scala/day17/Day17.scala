package day17

import java.util.concurrent.LinkedBlockingQueue

import common.IntCodeComputer._
import common._

object Day17 {

  sealed trait Turn

  case class Left() extends Turn

  case class Right() extends Turn

  def turnRequired(facingDirection: (Int, Int), targetDirection: (Int, Int)): Turn = {
    if ((facingDirection._2, - facingDirection._1) == targetDirection) Left()
    else Right()
  }

  type Coord = (Int, Int)

  val possibleDirections = List((1, 0), (-1, 0), (0, 1), (0, -1))

  def toMap(output: List[BigInt]): Map[Coord, Char] = output
    .map(_.toChar)
    .mkString("")
    .split('\n')
    .zipWithIndex
    .flatMap { case (row, y) => row
      .zipWithIndex
      .map { case (value, x) =>
        (x, y) -> value
      }
    }
    .toMap
    .withDefaultValue('.')

  def alignmentParametersSum(intCode: Array[BigInt]): Unit = {
    val input = new LinkedBlockingQueue[BigInt]()
    val output: List[BigInt] = compute(input)(State(intCode))
    println(output.map(_.toChar).mkString(""))
    val scaffoldMap = toMap(output)
    val result = scaffoldMap
      .filter(_._2 == '#')
      .filter { case (coord, _) => possibleDirections.map(coord + _).forall(scaffoldMap(_) == '#') }
      .keys
      .map(c => c._1 * c._2)
      .sum

    println(s"Sum of alignment parameters: $result")
  }
//
//  def traverseCompleteMap(intCode: Array[BigInt]): Unit = {
//    intCode(0) = 2
//    val input = new LinkedBlockingQueue[BigInt]()
//    val state = State(intCode)
//    val output: List[BigInt] = compute(input)(state)
//    val navigationMap = toMap(output)
//    val start = navigationMap.find(_._2 == '^').get._1
//    var facingDirect = (0, -1)
//
//    def traverse(coord: Coord = start): List[List[(Turn, Int)]] = {
//      possibleDirections.f
//    }
//  }

  def main(args: Array[String]): Unit = {
    val intCode = readIntCode("day17.txt")
    alignmentParametersSum(intCode)
//    traverseCompleteMap(intCode)
  }
}

/*
............................#######............
............................#.....#............
............................#.....#............
............................#.....#............
..........#.....#############.....#............
..........#.....#.................#............
..........#...#####...............#............
..........#...#.#.#...............#............
..........#.#########.............#............
..........#.#.#.#.#.#.............#............
....#######.#.#########...........#............
....#.......#...#.#.#.#...........#............
#############...#####.#...........#####........
#...#.............#...#...............#........
#...#.^############...#...............#........
#...#.................#...............#........
#...#.................#######.....#########....
#...#.......................#.....#...#...#....
#####.......................#.....#...#...#....
............................#.....#...#...#....
............................#.....#####...#....
............................#.............#....
............................#.............#....
............................#.............#....
............................#...#####.....#....
............................#...#...#.....#....
............................#...#...#.....#....
............................#...#...#.....#....
............................#########.....#####
................................#.............#
................................#.............#
................................#.............#
..........................#######.............#
..........................#...................#
..........................#.......#############
..........................#.......#............
..........................#.......#............
..........................#.......#............
..........................#########............

#######...#####
#.....#...#...#
#.....#...#...#
......#...#...#
......#...###.#
......#.....#.#
^########...#.#
......#.#...#.#
......#########
........#...#..
....#########..
....#...#......
....#...#......
....#...#......
....#####......

A,B,C,B,A,C
R,8,R,8
R,4,R,4,R,8
L,6,L,2
*/
