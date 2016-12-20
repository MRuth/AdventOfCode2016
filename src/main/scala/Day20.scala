import scala.annotation.tailrec
import scala.io.Source
/**
  * Created by Montana Ruth on 12/20/2016.
  */

object Day20 extends App {
  val max = 4294967295L
  val src = Source.fromFile("Files/Day20.txt")
  val in = src.getLines().toList
  src.close()
  case class Range(from: Long, to: Long)
  def merge(rs: List[Range]): List[Range] = {
    @tailrec
    def collapse(rs: List[Range], sep: List[Range] = Nil): List[Range] = rs match {
      case x :: y :: rest =>
        if (y.from > x.to+1) collapse(y :: rest, x :: sep)
        else collapse( Range(x.from, x.to max y.to) :: rest, sep)
      case _ =>
        (rs ::: sep).reverse
    }

    collapse(rs.sortBy(_.from))
  }

  val ranges = merge(in.map{str => val Array(from,to)=str.split("-").map(_.toLong);Range(from,to)})

  def part1: Unit = println(s"Part 1: ${
    (0L until ranges.head.from).headOption match {
      case Some(x) => x
      case _=> ranges.head.to+1
    }
  }")
  def part2: Unit = println(s"Part 2: ${
    ((0L until ranges.head.from) ++
      ranges.sliding(2).foldLeft(List[Long]()){case (lst, r1 :: r2 :: _) => lst ++ (r1.to + 1 until r2.from)} ++
      (ranges.last.to+1 to max)).size
  }")

  part1
  part2
}
