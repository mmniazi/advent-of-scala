package day7

import day7.Day7._
import org.scalatest.funsuite.AnyFunSuite

class Day7Test extends AnyFunSuite {

  test("test computeForPhaseSeq") {
    val phaseSeq = List(1, 0, 4, 3, 2)
    val intCode = Array(3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33,
      1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0)

    assert(computeForPhaseSeq(phaseSeq, intCode.map(BigInt(_))) == 65210)
  }

  test("test computeForPhaseSeqWithFeedback") {
    val phaseSeq = List(9, 7, 8, 5, 6)
    val intCode = Array(3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54,
      -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4,
      53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10)

    assert(computeForPhaseSeqWithFeedback(phaseSeq, intCode.map(BigInt(_))) == 18216)
  }
}
