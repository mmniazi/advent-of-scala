package day1

import org.scalatest.funsuite.AnyFunSuite
import Day1._
class Day1Test extends AnyFunSuite {

  test("fuel required for fuel without any further fuel requirements") {
    assert(fuelReqCorrected(2) == 0)
  }

  test("fuel required for fuel") {
    assert(fuelReqCorrected(100756) == 50346)
  }

  test("total fuel") {
    val fuelForComponents = fuelReq(100756)
    assert(fuelForComponents + fuelReqCorrected(fuelForComponents) == 50346)
  }
}
