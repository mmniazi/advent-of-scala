package day17

import java.util.concurrent.LinkedBlockingQueue

import common.IntCodeComputer._
import common._

object Day17 {
  type Coord = (Int, Int)
  type Direction = (Int, Int)
  type Move = (Char, Int)

  def left(facingDirection: Direction): Direction = (facingDirection._2, -facingDirection._1)

  def right(facingDirection: Direction): Direction = (-facingDirection._2, facingDirection._1)

  val possibleDirections = List((1, 0), (-1, 0), (0, 1), (0, -1))
  val scaffold = '#'

  def toMap(intCode: Array[BigInt]): Map[Coord, Char] = {
    val input = new LinkedBlockingQueue[BigInt]()
    val output: List[BigInt] = compute(input)(State(intCode))
    println(output.map(_.toChar).mkString(""))
    output
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
  }

  def alignmentParametersSum(intCode: Array[BigInt]): Unit = {
    val scaffoldMap = toMap(intCode)
    val sum = scaffoldMap
      .filter(_._2 == scaffold)
      .filter { case (coord, _) =>
        possibleDirections
          .map(dir => coord + dir)
          .forall(scaffoldMap(_) == scaffold)
      }
      .keys
      .map(c => c._1 * c._2)
      .sum

    println(s"Sum of alignment parameters: $sum")
  }

  def createPath(currPos: Coord, currDir: Direction, scaffoldMap: Map[Coord, Char], path: List[Move] = Nil): List[Move] = {
    Seq((currDir, 'F'), (left(currDir), 'L'), (right(currDir), 'R'))
      .find { case (dirVector, _) => scaffoldMap(currPos + dirVector) == scaffold }
      .map { case (dirVector, dirName) =>
        dirName match {
          case 'F' => createPath(currPos + dirVector, dirVector, scaffoldMap, (path.head._1, path.head._2 + 1) :: path.tail)
          case 'L' | 'R' => createPath(currPos + dirVector, dirVector, scaffoldMap, (dirName, 1) :: path)
        }
      }
      .getOrElse(path.reverse)
  }

  def traverseCompleteMap(intCode: Array[BigInt]): Unit = {
    val scaffoldMap = toMap(intCode)
    val start = scaffoldMap.find(_._2 == '^').get._1
    val initialFacingDirection = (0, -1)
    val path = createPath(start, initialFacingDirection, scaffoldMap)
    println(path)
  }

//  def groupPath(origPath: List[Move]): List[Char] = {
//    for ()
//  }

  def main(args: Array[String]): Unit = {
    val intCode = readIntCode("day17.txt")
    alignmentParametersSum(intCode)
    traverseCompleteMap(intCode)
  }
}
/*

(R,12), (L,8), (L,4), (L,4), (L,8), (R,6), (L,6), (R,12), (L,8), (L,4), (L,4), (L,8), (R,6), (L,6), (L,8), (L,4), (R,12), (L,6), (L,4), (R,12), (L,8), (L,4), (L,4), (L,8), (L,4), (R,12), (L,6), (L,4), (R,12), (L,8), (L,4), (L,4), (L,8), (L,4), (R,12), (L,6), (L,4), (L,8), (R,6), (L,6))

A, B, A, B, C, A, C, A, C, B


A: (R,12), (L,8), (L,4), (L,4)
B: (L,8), (R,6), (L,6)
C: (L,8), (L,4), (R,12), (L,6), (L,4)
*/