package twenty19.day18

import common._
import scala.io.Source

object Day18 {
  type Coord = (Int, Int)

  private val directions = (1, 0) :: (0, 1) :: (-1, 0) :: (0, -1) :: Nil
  private val noDirection = (0, 0)
  private val (start, empty, wall) = ('@', '.', '#')

  def readMap(input: List[String]): Map[Coord, Char] = input
    .zipWithIndex
    .flatMap { case (line, y) =>
      line.zipWithIndex.map { case (c, x) =>
        (x, y) -> c
      }
    }
    .toMap

  private def isDoor(char: Char) = char.isUpper

  private def isKey(char: Char) = char.isLower

  private def doorKey(char: Char) = char.toLower

  private def isWall(char: Char) = char == wall

  def startCoord(inputMap: Map[Coord, Char]): Coord = inputMap.find(_._2 == start).get._1

  def findPaths(start: Coord, inputMap: Map[Coord, Char], path: List[Coord],
                prevDir: Coord = noDirection): List[Coord] = {
    directions
      .filterNot(_ == prevDir)
      .map(d => start + d)
      .map(c => c -> inputMap(c))
      .collect {
        case (_, v) if isWall(v) => path
        case (coord, v) if isDoor(v) => coord :: path
        case (coord, _) => findPaths(coord, inputMap, coord :: path, coord - start)
      }
      .reduce(_ ++ _)
  }

  def main(args: Array[String]): Unit = {
    val inputMap: Map[Coord, Char] = readMap(Source.fromResource("day18.txt").getLines().toList)
  }
}
