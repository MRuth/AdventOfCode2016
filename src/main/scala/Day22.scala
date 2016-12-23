import scala.io.Source

/**
  * Created by Montana Ruth on 12/22/2016.
  */

object Day22 extends App {
  case class Node(x: Int, y: Int, size: Int, used: Int, avail: Int, usePercent: Double)
  val nodeR = """.+?node-x(\d+)-y(\d+).+?(\d+)T.+?(\d+)T.+?(\d+)T.+?(\d+)%""".r
  val src = Source.fromFile("Files/Day22.txt")
  val in = src.getLines.drop(2).toList
  src.close()
  val nodes = in.map{case nodeR(x,y,size,used,avail,usePercent) =>
    Node(x.toInt,y.toInt,size.toInt,used.toInt,avail.toInt,(usePercent.toDouble/100d))}

  def part1: Unit = println(s"Part 1: ${nodes.combinations(2).count{
    case a::b::Nil => (a != b && ((a.used != 0 && b.avail >= a.used) || (b.used != 0 && a.avail >= b.used)))
  }}")

  def part2: Unit = {
    val maxX = nodes.map(_.x).max
    val maxY = nodes.map(_.y).max
    val emptyNode = nodes.find(_.used == 0).get
    val goal = nodes.find(n => n.y == 0 && n.x == maxX).get
    val wallNodes = nodes.filter(n => n.size >= 250)
    val wallWithMinX = wallNodes.minBy(_.x)
    val ans = (maxX-1) * 5 + emptyNode.y + wallNodes.length + (emptyNode.x - wallWithMinX.x) + 1
    println(s"Part 2: ${ans}")
  }

  part1
  part2
}
