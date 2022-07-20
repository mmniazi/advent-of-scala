package twenty19.day14

import twenty19.day14.Day14._
import org.scalatest.funsuite.AnyFunSuite


class Day14Test extends AnyFunSuite {
  test("test oreRequired simple") {
    val lines =
    """9 ORE => 2 A
      |8 ORE => 3 B
      |7 ORE => 5 C
      |3 A, 4 B => 1 AB
      |5 B, 7 C => 1 BC
      |4 C, 1 A => 1 CA
      |2 AB, 3 BC, 4 CA => 1 FUEL"""
      .stripMargin.split('\n')
    val reactions = readReactions(lines)
    val result = oreRequired(reactions, Array(Chemical("FUEL", 1)))
    assert(result == 165)
  }

  test("test oreRequired complex") {
    val lines =
    """171 ORE => 8 CNZTR
      |7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
      |114 ORE => 4 BHXH
      |14 VRPVC => 6 BMBT
      |6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
      |6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
      |15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
      |13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
      |5 BMBT => 4 WPTQ
      |189 ORE => 9 KTJDG
      |1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
      |12 VRPVC, 27 CNZTR => 2 XDBXC
      |15 KTJDG, 12 BHXH => 5 XCVML
      |3 BHXH, 2 VRPVC => 7 MZWV
      |121 ORE => 7 VRPVC
      |7 XCVML => 6 RJRHP
      |5 BHXH, 4 VRPVC => 5 LTCX"""
      .stripMargin.split('\n')
    val reactions = readReactions(lines)
    val result = oreRequired(reactions, Array(Chemical("FUEL", 1)))
    assert(result == 2210736)
  }

  test("test fuelProduced") {
    val lines =
    """171 ORE => 8 CNZTR
      |7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
      |114 ORE => 4 BHXH
      |14 VRPVC => 6 BMBT
      |6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
      |6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
      |15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
      |13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
      |5 BMBT => 4 WPTQ
      |189 ORE => 9 KTJDG
      |1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
      |12 VRPVC, 27 CNZTR => 2 XDBXC
      |15 KTJDG, 12 BHXH => 5 XCVML
      |3 BHXH, 2 VRPVC => 7 MZWV
      |121 ORE => 7 VRPVC
      |7 XCVML => 6 RJRHP
      |5 BHXH, 4 VRPVC => 5 LTCX"""
      .stripMargin.split('\n')
    val reactions = readReactions(lines)
    val result = fuelProduced(reactions)
    assert(result == 460664)
  }
}
