package day7

import java.util.concurrent.LinkedBlockingQueue

import common.IntCodeComputer._

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._

object Day7 {

  @tailrec
  def computeForPhaseSeq(phaseSequence: Seq[Int], intCode: Array[BigInt], input: BigInt = 0): BigInt = phaseSequence match {
    case head :: tail =>
      val output = compute(List(BigInt(head), input))(State(intCode)).head
      computeForPhaseSeq(tail, intCode, output)
    case Nil => input
  }

  /**
   * Input and outputs of amplifiers can be treated as producer consumer problem
   *
   * @return blocking input output queues linked with each other in sequence forming a feedback loop
   */
  def createFeedbackLoop(phaseSequence: Seq[Int]): Seq[(LinkedBlockingQueue[BigInt], LinkedBlockingQueue[BigInt])] = {
    val inputIndexes = List(4, 0, 1, 2, 3)
    val phaseWithOutputs = phaseSequence.map((_, new LinkedBlockingQueue[BigInt]))
    val inputOutputs = for (((phase, outputQueue), index) <- phaseWithOutputs.zipWithIndex) yield {
      val inputQueue = phaseWithOutputs(inputIndexes(index))._2
      inputQueue.add(phase)
      (inputQueue, outputQueue)
    }
    inputOutputs.head._1.add(0) // Add input to first amplifier to kick off process
    inputOutputs
  }

  def computeForPhaseSeqWithFeedback(phaseSequence: Seq[Int], intCode: Array[BigInt]): BigInt = {
    createFeedbackLoop(phaseSequence).par.map { case (inputQueue, outputQueue) =>
      compute(inputQueue, outputQueue)(State(intCode))
    }.last.last
  }

  def main(args: Array[String]): Unit = {
    val intCode = readIntCode("day7.txt")

    val possiblePhaseSequences = (0 to 4).toList.permutations.toList
    val possibleOutputs = possiblePhaseSequences.par.map(computeForPhaseSeq(_, intCode))
    println(s"highest output signal: ${possibleOutputs.max}")

    val possiblePhaseSequencesFeedback = (5 to 9).toList.permutations
    val possibleOutputsFeedback = possiblePhaseSequencesFeedback.map(computeForPhaseSeqWithFeedback(_, intCode))
    println(s"highest output signal with feedback loop: ${possibleOutputsFeedback.max}")
  }
}
