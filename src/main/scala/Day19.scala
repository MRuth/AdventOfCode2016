import scala.collection.mutable.ArrayBuffer

/**
  * Created by Montana Ruth on 12/18/2016.
  */

object Day19 extends App {
  val in = 3018458
  def part1: Unit = println(s"Part 1: ${(2 * (in-Integer.highestOneBit(in)) + 1)}")
  def part2: Unit = println(s"Part 2: ${
    Math.pow(3,Math.floor(Math.log(in)/Math.log(3))).toInt match {
      case x if (x == in) => x
      case x if (in - x  <= x) => in-x
      case x => 2 * in - 3 * x
    }}")

  part1
  part2

}
