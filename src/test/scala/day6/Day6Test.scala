package day6

import day6.Day6._
import org.scalatest.funsuite.AnyFunSuite

class Day6Test extends AnyFunSuite {

  test("test createTree") {
    val nodes = List(("COM", "B"), ("B", "C"), ("C", "D"), ("B", "G"), ("G", "H"))
    val (tree, _) = createTree(nodes)
    val expected = "COM(B(C(D()),G(H())))"
    assert(tree.toString == expected)
  }

  test("test calculateOrbits") {
    val nodes = List(("COM", "B"), ("B", "C"), ("C", "D"), ("D", "E"), ("E", "F"), ("B", "G"),
      ("G", "H"), ("D", "I"), ("E", "J"), ("J", "K"), ("K", "L"))
    val (tree, _) = createTree(nodes)
    assert(calculateOrbits(tree) == 42)
  }

  test("test calculateDistance") {
    val nodes = List(("COM","B"), ("B","C"), ("C","D"), ("D","E"), ("E","F"), ("B","G"), ("G","H"),
      ("D","I"), ("E","J"), ("J","K"), ("K","L"), ("K","YOU"), ("I","SAN"))
    val (_, map) = createTree(nodes)
    assert(calculateDistance(Some(map("YOU")), Some(map("SAN"))) == 4)
  }
}
