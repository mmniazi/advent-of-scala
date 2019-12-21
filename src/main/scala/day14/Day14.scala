package day14

import scala.collection.mutable
import scala.io.Source

object Day14 {

  case class Chemical(name: String, quantity: BigInt)

  case class Reaction(outputQuantity: BigInt, inputs: Seq[Chemical])

  def posDiff(i1: BigInt, i2: BigInt): BigInt = if (i1 > i2) i1 - i2 else BigInt(0)

  def divideCeil(i1: BigInt, i2: BigInt): BigInt = (i1 / i2) + (if (i1 % i2 == 0) 0 else 1)

  def oreRequired(reactions: Map[String, Reaction], requiredChemicals: Seq[Chemical], extraChemicals: mutable.Map[String, BigInt] = mutable.Map().withDefaultValue(0)): BigInt = {
    requiredChemicals.map { chemical =>
      if (chemical.name == "ORE")
        chemical.quantity
      else {
        val reaction = reactions(chemical.name)
        val quantityRequired = posDiff(chemical.quantity, extraChemicals(chemical.name))
        extraChemicals(chemical.name) = posDiff(extraChemicals(chemical.name), chemical.quantity)
        extraChemicals(chemical.name) += divideCeil(quantityRequired, reaction.outputQuantity) * reaction.outputQuantity - quantityRequired
        oreRequired(reactions, reaction.inputs.map(c =>
          Chemical(c.name, c.quantity * divideCeil(quantityRequired, reaction.outputQuantity))
        ), extraChemicals)
      }
    }.sum
  }

  def fuelProduced(reactions: Map[String, Reaction]): BigInt = {
    var oreLeft = BigInt("1000000000000")
    val oreForUnitFuel: BigInt = oreRequired(reactions, Seq(Chemical("FUEL", 1))).toInt
    val extraChemicals = mutable.Map[String, BigInt]().withDefaultValue(0)
    var fuelsProducedWithWastage = oreLeft / oreForUnitFuel
    var fuel = BigInt(0)

    while (fuelsProducedWithWastage != 0) {
      val oreConsumed = oreRequired(reactions, Seq(Chemical("FUEL", fuelsProducedWithWastage.toInt)), extraChemicals)
      oreLeft -= oreConsumed
      fuel += fuelsProducedWithWastage
      fuelsProducedWithWastage = oreLeft / oreForUnitFuel
    }

    fuel
  }

  def readReactions(lines: Seq[String]): Map[String, Reaction] = {
    Map.from(lines.map { l =>
      val input = l.split("=>")(0)
      val output = l.split("=>")(1)

      def toQuantityChemical(s: String) = {
        val r = s.trim.split(" ")
        Chemical(r(1), BigInt(r(0)))
      }

      val outputChemical = toQuantityChemical(output)
      val inputChemicals = input.split(",").map(toQuantityChemical)
      outputChemical.name -> Reaction(outputChemical.quantity, inputChemicals)
    })
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day14.txt").getLines().toSeq
    val reactions = readReactions(lines)
    println(s"OREs required: ${oreRequired(reactions, Seq(Chemical("FUEL", 1)))}")
    println(s"fuel produced: ${fuelProduced(reactions)}")
  }
}