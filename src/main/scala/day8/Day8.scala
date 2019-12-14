package day8

import scala.io.Source

object Day8 {
  val WIDTH: Int = 25
  val HEIGHT: Int = 6

  def calculateHash(input: Seq[Int], imageSize: Int): Int = {
    val layerWithMinZeroes = input.grouped(imageSize).maxBy(-_.count(_ == 0))
    layerWithMinZeroes.count(_ == 1) * layerWithMinZeroes.count(_ == 2)
  }

  def decodeImage(input: Seq[Int], imageSize: Int): List[Int] =
    input.grouped(imageSize).toList.transpose.map(_.find(_ != 2).getOrElse(2))

  def prettyPrint(output: List[Int]): String = {
    output.map {
      case 0 => "◼️"
      case 1 => "◻️"
      case 2 => " "
    }.grouped(WIDTH).map(_.mkString("")).mkString("\n")
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("day8.txt")
    val input = source.getLines().toList.head.map(_.asDigit)
    println(s"part 1 result: ${calculateHash(input, WIDTH * HEIGHT)}")
    println(s"part 2 result: \n${prettyPrint(decodeImage(input, WIDTH * HEIGHT))}")
  }
}
