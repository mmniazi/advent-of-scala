package day7

import day5.Day5.{ValueProvider, compute}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._
import scala.io.Source


class FeedbackValueProvider(phaseSequence: mutable.Queue[Int]) extends ValueProvider {
  private var inputCounter = -1

  override def enqueue(value: Int): ValueProvider =
    super.enqueue(value)

  override def dequeue: Int = {
    inputCounter += 1
    if (inputCounter % 2 == 0 && phaseSequence.nonEmpty) {
      phaseSequence.dequeue()
    }
    else super.dequeue
  }

  override def toList: List[Int] = super.toList
}

object Day7 {

  @tailrec
  def computeForPhaseSeq(phaseSequence: Seq[Int], intCode: Array[Int], input: Int = 0): Int = phaseSequence match {
    case head :: tail =>
      val output = compute(ValueProvider(head, input))(intCode).head
      computeForPhaseSeq(tail, intCode, output)
    case Nil => input
  }

  def computeForPhaseSeqWithFeedback(phaseSequence: Seq[Int], intCode: Array[Int], input: Int = 0): Int = {
    val inputOutput = new FeedbackValueProvider(mutable.Queue.from(phaseSequence))
    inputOutput.enqueue(0)
    val res = compute(input = inputOutput, output = inputOutput)(intCode)
    res.last
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("day7.txt")
    val intCode: Array[Int] = source.getLines().toList.head.split(',').map(_.toInt)

    val possiblePhaseSequences: List[List[Int]] = (0 to 4).toList.permutations.toList
    val possibleOutputs = possiblePhaseSequences.par.map(computeForPhaseSeq(_, intCode.clone()))
    println(s"highest output signal: ${possibleOutputs.max}")

    val possiblePhaseSequencesFeedback = (5 to 9).toList.permutations
    val possibleOutputsFeedback = possiblePhaseSequencesFeedback.map(computeForPhaseSeqWithFeedback(_, intCode.clone()))
    println(s"highest output signal with feedback loop: ${possibleOutputsFeedback.max}")
  }
}
