package day5

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.jdk.CollectionConverters._

object Day5 {

  def readIntCode(dayFile: String): Array[BigInt] = Source.fromResource(dayFile).getLines().toList.head.split(',').map(BigInt(_))

  implicit def listToQueue(l: List[BigInt]): LinkedBlockingQueue[BigInt] = new LinkedBlockingQueue[BigInt](l.asJava)

  case class State(intCode: Array[BigInt], var pointer: BigInt = 0, var relativeBase: BigInt = 0) {
    val memory: mutable.Map[BigInt, BigInt] = mutable.Map(intCode.zipWithIndex.map(t => (BigInt(t._2), t._1)): _*).withDefaultValue(0)
  }

  @tailrec
  def compute(input: LinkedBlockingQueue[BigInt], output: LinkedBlockingQueue[BigInt] = new LinkedBlockingQueue())(implicit state: State): List[BigInt] = {
    Op(state.memory(state.pointer).toInt) match {
      case op@Op(_, 1) =>
        op.p3 = op.p1 + op.p2
        state.pointer += 4
        compute(input, output)

      case op@Op(_, 2) =>
        op.p3 = op.p1 * op.p2
        state.pointer += 4
        compute(input, output)

      case op@Op(_, 3) =>
        op.p1 = input.poll(1, TimeUnit.DAYS)
        state.pointer += 2
        compute(input, output)

      case op@Op(_, 4) =>
        output.add(op.p1)
        state.pointer += 2
        compute(input, output)

      case op@Op(_, 5) =>
        if (op.p1 != 0) {
          state.pointer = op.p2
          compute(input, output)
        }
        else {
          state.pointer += 3
          compute(input, output)
        }

      case op@Op(_, 6) =>
        if (op.p1 == 0) {
          state.pointer = op.p2
          compute(input, output)
        }
        else {
          state.pointer += 3
          compute(input, output)
        }

      case op@Op(_, 7) =>
        op.p3 = if (op.p1 < op.p2) 1 else 0
        state.pointer += 4
        compute(input, output)

      case op@Op(_, 8) =>
        op.p3 = if (op.p1 == op.p2) 1 else 0
        state.pointer += 4
        compute(input, output)

      case op@Op(_, 9) =>
        state.relativeBase += op.p1
        state.pointer += 2
        compute(input, output)

      case _ => output.asScala.toList
    }
  }

  object Op {
    def apply(code: Int): Op = new Op(code, code % 100)
  }

  case class Op(code: Int, opType: Int) {
    val (p1mode, p2mode, p3mode) = (code % 1000 / 100, code % 10000 / 1000, code / 10000)

    private def index(mode: Int, increment: BigInt)(implicit state: State): BigInt = mode match {
      case 0 => state.memory(state.pointer + increment)
      case 1 => state.pointer + increment
      case 2 => state.relativeBase + state.memory(state.pointer + increment)
    }

    def p1(implicit state: State): BigInt = state.memory(index(p1mode, 1))

    def p2(implicit state: State): BigInt = state.memory(index(p2mode, 2))

    def p3(implicit state: State): BigInt = state.memory(index(p3mode, 3))

    def p1_=(value: BigInt)(implicit state: State): Unit = state.memory(index(p1mode, 1)) = value

    def p3_=(value: BigInt)(implicit state: State): Unit = state.memory(index(p3mode, 3)) = value
  }

  def main(args: Array[String]): Unit = {
    val intCode = readIntCode("day5.txt")
    val firstPartOutput = compute(input = BigInt(1) :: Nil)(State(intCode))
    println(s"part1 diagnostic code: ${firstPartOutput.last}")
    val secondPartOutput = compute(input = BigInt(5) :: Nil)(State(intCode))
    println(s"part2 diagnostic code: ${secondPartOutput.last}")
  }
}
