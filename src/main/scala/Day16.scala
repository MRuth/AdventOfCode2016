
/**
  * Created by Montana Ruth on 12/16/2016.
  */

object Day16 extends App {
  val in = "01110110101001000"
  def dragonCurve(in: String)(len: Int): String =
    Iterator.iterate(in){a => a + "0" + a.reverse.map(c => if (c == '0') '1' else '0')}.
    dropWhile(_.length < len).next().take(len)
  def checksum(in: String): String = {
    val grps = Iterator.from(3,2).dropWhile(in.length % _ != 0).next
    in.grouped(in.length/grps).map{s => if(s.count(_ == '1') % 2 == 0) "1" else "0"}.mkString
  }

  def part1: Unit =
    println(s"Part 1: ${checksum(dragonCurve(in)(272))}")

  def part2: Unit =
    println(s"Part 2: ${checksum(dragonCurve(in)(35651584))}")

  part1
  part2
}