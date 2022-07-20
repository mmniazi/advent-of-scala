package twenty19.day6

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day6 {

  case class Node(name: String, var parent: Option[Node] = None, children: mutable.ListBuffer[Node] = ListBuffer(), var distance: Int = 0) {
    override def toString: String = s"$name(${children.mkString(",")})"
  }

  @tailrec
  def createTree(nodes: List[(String, String)], nodesMap: mutable.Map[String, Node] = mutable.Map()): (Node, mutable.Map[String, Node]) = {
    nodes match {
      case (parent, child) :: tail =>
        val childNode = nodesMap.getOrElseUpdate(child, Node(child))
        val parentNode = nodesMap.getOrElseUpdate(parent, Node(parent))
        parentNode.children.append(childNode)
        childNode.parent = Some(parentNode)
        createTree(tail, nodesMap)
      case Nil =>
        (nodesMap.values.find(_.parent.isEmpty).get, nodesMap)
    }
  }

  def calculateOrbits(node: Node, distanceFromRoot: Int = 0): Int = {
    distanceFromRoot + node.children.map(calculateOrbits(_, distanceFromRoot + 1)).sum
  }

  def calculateDistance(nodeFromYou: Option[Node], nodeFromSan: Option[Node], step: Int = 0): Int = {
    for (node <- List(nodeFromYou, nodeFromSan) if node.isDefined)
      if (node.get.distance == 0) node.get.distance = step
      else return node.get.distance + step - 2

    calculateDistance(nodeFromYou.flatMap(_.parent), nodeFromSan.flatMap(_.parent), step + 1)
  }

  def main(args: Array[String]): Unit = {
    val nodes: List[(String, String)] = Source.fromResource("day6.txt").getLines().map(_.split(')')).map(r => (r(0), r(1))).toList

    val (tree, map) = createTree(nodes)
    println(calculateOrbits(tree))
    println(calculateDistance(Some(map("YOU")), Some(map("SAN"))))
  }
}
