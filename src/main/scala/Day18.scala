
/**
  * Created by Montana Ruth on 12/17/2016.
  */

object Day18 extends App {
  val in = ".^..^....^....^^.^^.^.^^.^.....^.^..^...^^^^^^.^^^^.^.^^^^^^^.^^^^^..^.^^^.^^..^.^^.^....^.^...^^.^."
  def isSafe(l: Char, r: Char): Char = if(l == r) '.' else '^'

  def iter = Iterator.iterate((in,0L)){case (tiles,count) =>
    (s".$tiles.".sliding(3).map{s => isSafe(s(0),s(2))}.mkString,count+tiles.count(_ == '.'))}

  def part1: Unit = println(s"Part 1: ${iter.drop(40).next._2}")
  def part2: Unit = println(s"Part 2: ${iter.drop(4000000).next._2}")

  part1
  part2
  println(s"Execution Time: ${System.currentTimeMillis-executionStart} ms")
}