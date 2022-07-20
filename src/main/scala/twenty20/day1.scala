package twenty20

import scala.annotation.tailrec
import scala.io.Source

object day1 {
  @tailrec
  def sumUpto2020(numbers: List[Int], seenNumbersMap: Set[Int] = Set()): Int = {
    if (numbers.isEmpty) return 0
    val toFind = 2020 - numbers.head
    if (seenNumbersMap contains toFind) numbers.head * toFind
    else sumUpto2020(numbers.tail, seenNumbersMap + numbers.head)
  }

  def main(args: Array[String]): Unit = {
    val numbers = Source.fromResource("twenty20/day1.txt").getLines().map(_.toInt).toList
    println(sumUpto2020(numbers))
  }
}
