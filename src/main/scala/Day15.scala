import scala.io.Source

/**
  * Created by Montana Ruth on 12/15/2016.
  */

object Day15 extends App {
  val parseR = """.+ (\d+) .+ (\d+)""".r
  val src = Source.fromFile("Files/Day15.txt")
  val in = src.getLines().flatMap(parseR.findFirstMatchIn(_).collect{case m => (m.group(1).toInt,m.group(2).toInt)}).toList
  src.close()

  def findTime(in: List[(Int,Int)]): Option[Int] =
    Iterator.iterate(1)(_+1).find{t => in.zipWithIndex.forall{case ((nPos,cPos), i) => ((cPos+(i+1)+t) % nPos) == 0}}

  def part1: Unit = println(s"Part 1: ${findTime(in).get}")

  def part2: Unit = println(s"Part 2: ${findTime((in :+ (11,0))).get}")

  part1
  part2
}