package day4

import scala.annotation.tailrec

object Day4 {

  @tailrec
  def hasIncreasingDigits(number: String): Boolean = {
    if (number.length == 1) true
    else if (number.head > number.tail.head) false
    else hasIncreasingDigits(number.tail)
  }

  @tailrec
  def groupConsecutive(list: List[Char], accumulator: List[(Char, Int)] = List()): List[(Char, Int)] = list match {
    case head :: tail =>
      val (equalsHead, notEqualsHead) = tail.span(_ == head)
      groupConsecutive(notEqualsHead, accumulator :+ (head, equalsHead.size + 1))
    case _ => accumulator
  }

  def hasConsecutiveDuplicatePart1(number: String): Boolean =
    number
      .toList
      .sliding(2)
      .exists(v => v.head == v.tail.head)

  def hasConsecutiveDuplicatePart2(number: String): Boolean =
    groupConsecutive(number.toList)
      .exists(_._2 == 2)


  def main(args: Array[String]): Unit = {
    val min = 152085
    val max = 670283

    def passwordCandidates(hasConsecutiveDuplicates: String => Boolean) = for {
      i <- min to max
      if hasConsecutiveDuplicates(i.toString)
      if hasIncreasingDigits(i.toString)
    } yield i

    println(s"part 1 solution: ${passwordCandidates(hasConsecutiveDuplicatePart1).size}")
    println(s"part 2 solution: ${passwordCandidates(hasConsecutiveDuplicatePart2).size}")
  }
}
