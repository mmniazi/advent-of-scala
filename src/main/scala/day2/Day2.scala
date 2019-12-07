package day2

import scala.annotation.tailrec
import scala.io.Source

object Day2 {

  private def update(input: Array[Int], pointer: Int, op: (Int, Int) => Int) = {
    input(input(pointer + 3)) = op(input(input(pointer + 1)), input(input(pointer + 2)))
    input
  }

  @tailrec
  def compute(input: Array[Int], pointer: Int = 0): Array[Int] = {
    input.lift(pointer) match {
      case Some(1) => compute(update(input, pointer, _ + _), pointer + 4)
      case Some(2) => compute(update(input, pointer, _ * _), pointer + 4)
      case Some(99) => input
      case Some(_) => throw new Exception()
      case None => input
    }
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("day2.txt")
    val input: Array[Int] = source.getLines().toList.head.split(',').map(_.toInt)

    println {
      val copy = input.clone()
      copy(1) = 12
      copy(2) = 2
      s"part 1 solution: ${compute(copy).head}"
    }

    val expected = 19690720
    for (noun <- 0 to 99) {
      for (verb <- 0 to 99) {
        val copy = input.clone()
        copy(1) = noun
        copy(2) = verb
        if (compute(copy).head == expected)
          println(s"part 2 solution: ${100 * noun + verb}")
      }
    }
  }
}
