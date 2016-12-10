import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by Montana Ruth on 12/7/2016.
  */

object Day8 extends App {
    val screen = ArrayBuffer.fill(6)(ArrayBuffer.fill(50)(' '))
    def rect(a: Int, b: Int) = for(r <- 0 until b; c <- 0 until a) screen(r)(c) = '#'
    def rotateRow(row: Int, n: Int) =
      screen(row)=screen(row).zipWithIndex.map(t => (t._1,((t._2+n)%screen(row).length))).sortBy(_._2).map(_._1)
    def rotateCol(col: Int, n: Int) = {
      val target = screen.transpose.apply(col)
      val aShift = target.zipWithIndex.map(t => (t._1,((t._2+n)%target.length))).sortBy(_._2).map(_._1)
      aShift.zipWithIndex.foreach(t => screen(t._2)(col) = t._1)
    }
  val rectR = """rect (\d+)x(\d+)""".r
  val rotateRowR = """rotate row y=(\d+) by (\d+)""".r
  val rotateColR = """rotate column x=(\d+) by (\d+)""".r
  val src = Source.fromFile("Files/Day8.txt")
  val in = src.getLines().toList
  src.close()

  in.foreach{str => str match{
    case rectR(a,b) => rect(a.toInt,b.toInt)
    case rotateRowR(r,s) => rotateRow(r.toInt,s.toInt)
    case rotateColR(c,s) => rotateCol(c.toInt,s.toInt)
  }}

  def part1: Unit = {
    val ans = screen.flatten.count(_ == '#')
    println(s"Part1: $ans")
  }

  def part2: Unit = {
    println("Part 2:\n")
    screen.foreach{r => r.grouped(5).foreach{sec => print(sec.mkString+"  ")};println}
  }

  part1
  part2
}