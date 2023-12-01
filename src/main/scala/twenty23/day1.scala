package twenty23

import scala.io.Source

object day1 {

  private val replacements = Map(
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)
  ) ++ (1 to 9).map(i => (i.toString, i))

  private def part1(lines: List[String]): Int = lines
    .map(_.filter(_.isDigit).map(_.asDigit))
    .map(l => List(l.head, l.last).mkString.toInt)
    .sum

  private def part2(lines: List[String]): Int =
    lines
    .map { line =>
      val first = replacements
        .keys
        .map(n => (line.indexOf(n), n))
        .filter(_._1 != -1)
        .minBy(_._1)
        ._2
      val last = replacements
        .keys
        .map(n => (line.lastIndexOf(n), n))
        .filter(_._1 != -1)
        .maxBy(_._1)
        ._2
      replacements(first) * 10 + replacements(last)
    }
    .sum

  def main(args: Array[String]): Unit = {
    val lines = Source
      .fromResource("twenty23/day1.txt")
      .getLines()
      .toList

    println(part1(lines))
    println(part2(lines))
  }
}