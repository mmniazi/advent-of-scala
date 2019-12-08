package day5

import scala.annotation.tailrec
import scala.io.Source

object Day5 {
  object Op { def apply(code: Int): Op = new Op(code, code % 100) }
  case class Op(code: Int, opType: Int) {
    val (p1m, p2m, p3m) = (code % 1000 / 100, code % 10000 / 1000, code / 10000)
    def p1(implicit intCode: Array[Int], pointer: Int): Int = if (p1m == 1) pointer + 1 else intCode(pointer + 1)
    def p2(implicit intCode: Array[Int], pointer: Int): Int = if (p2m == 1) pointer + 2 else intCode(pointer + 2)
    def p3(implicit intCode: Array[Int], pointer: Int): Int = if (p3m == 1) pointer + 3 else intCode(pointer + 3)
  }

  @tailrec
  def compute(implicit intCode: Array[Int], input: List[Int], output: List[Int] = Nil, pointer: Int = 0): List[Int] =
    intCode
      .lift(pointer)
      .map(Op(_))
    match {
      case Some(op) if op.opType == 1 =>
        intCode(op.p3) = intCode(op.p1) + intCode(op.p2)
        compute(intCode, input, output, pointer + 4)

      case Some(op) if op.opType == 2 =>
        intCode(op.p3) = intCode(op.p1) * intCode(op.p2)
        compute(intCode, input, output, pointer + 4)

      case Some(op) if op.opType == 3 =>
        intCode(op.p1) = input.head
        compute(intCode, input.tail, output, pointer + 2)

      case Some(op) if op.opType == 4 =>
        compute(intCode, input, output :+ intCode(op.p1), pointer + 2)

      case Some(op) if op.opType == 5 =>
        if (intCode(op.p1) != 0) compute(intCode, input, output, intCode(op.p2))
        else compute(intCode, input, output, pointer + 3)

      case Some(op) if op.opType == 6 =>
        if (intCode(op.p1) == 0) compute(intCode, input, output, intCode(op.p2))
        else compute(intCode, input, output, pointer + 3)

      case Some(op) if op.opType == 7 =>
        intCode(op.p3) = if (intCode(op.p1) < intCode(op.p2)) 1 else 0
        compute(intCode, input, output, pointer + 4)

      case Some(op) if op.opType == 8 =>
        intCode(op.p3) = if (intCode(op.p1) == intCode(op.p2)) 1 else 0
        compute(intCode, input, output, pointer + 4)

      case Some(op) if op.opType == 99 => output

      case Some(_) => throw new Exception()

      case None => output
    }

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("day5.txt")
    val intCode: Array[Int] = source.getLines().toList.head.split(',').map(_.toInt)
    val firstPartOutput = compute(intCode.clone(), input = 1 :: Nil)
    println(s"part1 diagnostic code: ${firstPartOutput.last}")
    val secondPartOutput = compute(intCode.clone(), input = 5 :: Nil)
    println(s"part2 diagnostic code: ${secondPartOutput.last}")
  }
}
