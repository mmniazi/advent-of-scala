package twenty19.day16

import scala.annotation.tailrec
import scala.io.Source

object Day16 {
  val basePattern: LazyList[Int] = LazyList(0, 1, 0, -1) #::: basePattern

  def toOutput(input: List[Int], offset: Int = 0): String = input.slice(offset, offset + 8).mkString("")

  def offset(input: List[Int]): Int = input.take(7).mkString("").toInt

  @tailrec
  def nthPhase(input: List[Int], times: Int): List[Int] = {
    if (times == 0) return input
    val result = List.fill(input.size)(input).zipWithIndex.map { case (list, index) =>
      val pattern: LazyList[Int] = basePattern.flatMap(LazyList.fill(index + 1)(_)).tail
      val sum = list.zip(pattern).map { case (e, p) => e * p }.sum
      math.abs(sum) % 10
    }
    nthPhase(result, times - 1)
  }
  /**
   * For given input lastN digits are part of last 25% digits so multiply pattern for these would look like this:
   * a*0 b*0 c*1 d*1 e*1 = N - 2
   * a*0 b*0 c*0 d*1 e*1 = N - 1
   * a*0 b*0 c*0 d*0 e*1 = N
   * So we can calculate (N - i)th value for next phase by summing last (i + 1) values from current phase
   * Similarly (N - i - 1)th value for next phase would be sum of last (i + 2) values from prev phase
   * Or we can say (N - i - 1)th value for next phase would be: (N - i)th value for next phase + last (i + 2)th value from current phase
   * Hence the formula: Next phase (i) = Current phase (i) + Next phase (i + 1)*/
  def nthPhase10000(input: List[Int], times: Int): List[Int] = {
    val repeatedInput = Array.fill(10000)(input).flatten
    val lastN = repeatedInput.length - offset(input)
    val relevantInput = repeatedInput.takeRight(lastN + 1)
    for (_ <- 0 until 100) {
      for (i <- (0 until lastN).reverse) {
        relevantInput(i) = (relevantInput(i) + relevantInput(i + 1)) % 10
      }
    }
    relevantInput.tail.toList
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day16.txt").getLines().toList.head.toList.map(_.asDigit)
    println(s"output after 100 phases: ${toOutput(nthPhase(input, 100))}")
    println(s"output after 100 phases: ${toOutput(nthPhase10000(input, 100))}")
  }
}