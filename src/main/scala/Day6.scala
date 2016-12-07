import scala.io.Source

/**
  * Created by Montana Ruth on 12/5/2016.
  */

object Day6 extends App {
  val src = Source.fromFile("Files/Day6.txt")
  val in = src.getLines.toList
  src.close

  def part1(): Unit =
    println(s"Part 1: ${in.transpose.map{l=> l.groupBy(identity).toSeq.sortBy(m => -m._2.length).head._1}.mkString}")

  def part2(): Unit=
    println(s"Part 2: ${in.transpose.map{l => l.groupBy(identity).toSeq.sortBy(l2 => l2._2.length).head._1}.mkString}")

  part1()
  part2()
}