package day5

import scala.annotation.tailrec
import scala.io.Source

object Day5 {
  @tailrec
  def compute(input: List[Int], output: List[Int] = Nil)(implicit intCode: Array[Int], pointer: Int = 0): List[Int] =
    Op(intCode(pointer)) match {
      case op@Op(_, 1) =>
        op.p3 = op.p1 + op.p2
        compute(input, output)(intCode, pointer + 4)

      case op@Op(_, 2) =>
        op.p3 = op.p1 * op.p2
        compute(input, output)(intCode, pointer + 4)

      case op@Op(_, 3) =>
        op.p1 = input.head
        compute(input.tail, output)(intCode, pointer + 2)

      case op@Op(_, 4) =>
        compute(input, output :+ op.p1)(intCode, pointer + 2)

      case op@Op(_, 5) =>
        if (op.p1 != 0) compute(input, output)(intCode, op.p2)
        else compute(input, output)(intCode, pointer + 3)

      case op@Op(_, 6) =>
        if (op.p1 == 0) compute(input, output)(intCode, op.p2)
        else compute(input, output)(intCode, pointer + 3)

      case op@Op(_, 7) =>
        op.p3 = if (op.p1 < op.p2) 1 else 0
        compute(input, output)(intCode, pointer + 4)

      case op@Op(_, 8) =>
        op.p3 = if (op.p1 == op.p2) 1 else 0
        compute(input, output)(intCode, pointer + 4)

      case _ => output
    }

  object Op {
    def apply(code: Int): Op = new Op(code, code % 100)
  }

  case class Op(code: Int, opType: Int) {
    val (p1mode, p2mode, p3mode) = (code % 1000 / 100, code % 10000 / 1000, code / 10000)
    def p1(implicit intCode: Array[Int], p: Int): Int = intCode(if (p1mode == 1) p + 1 else intCode(p + 1))
    def p2(implicit intCode: Array[Int], p: Int): Int = intCode(if (p2mode == 1) p + 2 else intCode(p + 2))
    def p3(implicit intCode: Array[Int], p: Int): Int = intCode(if (p3mode == 1) p + 3 else intCode(p + 3))
    def p1_=(value: Int)(implicit intCode: Array[Int], p: Int): Unit = intCode(if (p1mode == 1) p + 1 else intCode(p + 1)) = value
    def p3_=(value: Int)(implicit intCode: Array[Int], p: Int): Unit = intCode(if (p3mode == 1) p + 3 else intCode(p + 3)) = value
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("day5.txt")
    val intCode: Array[Int] = source.getLines().toList.head.split(',').map(_.toInt)
    val firstPartOutput = compute(input = 1 :: Nil)(intCode.clone())
    println(s"part1 diagnostic code: ${firstPartOutput.last}")
    val secondPartOutput = compute(input = 5 :: Nil)(intCode.clone())
    println(s"part2 diagnostic code: ${secondPartOutput.last}")
  }
}
