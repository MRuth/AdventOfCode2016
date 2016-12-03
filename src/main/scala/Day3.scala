import scala.io.Source

/**
  * Created by Montana Ruth on 12/3/2016.
  */

object Day3 extends App {
  case class Triangle(a: Int, b: Int, c:Int)
  def isValidTriangle(t: Triangle) = ((t.a+t.b > t.c) && (t.b+t.c > t.a) && (t.a+t.c > t.b))
  val src = Source.fromFile("Files/Day3.txt")
  val in = src.getLines().toList; src.close()
  val regex = """\s+(\d+)\s+(\d+)\s+(\d+)""".r

  def part1(): Unit =
    println(s"Part 1: ${
      in.map{str => val regex(a,b,c) = str; Triangle(a.toInt,b.toInt,c.toInt)}.count(isValidTriangle)
    }")

  def part2(): Unit =
    println(s"Part 2: ${in.map(_.trim.split("""\s+""").map(_.toInt)).transpose.flatMap{
        lst => lst.grouped(3).map{case a::b::c::Nil => Triangle(a,b,c)}}
      .count(isValidTriangle)
    }")

  part1()
  part2()
}