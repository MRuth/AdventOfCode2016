import scala.annotation.tailrec
import scala.collection.SeqLike
import scala.io.Source

/**
  * Created by Montana Ruth on 12/1/2016.
  */

object Day1 extends App {
  val src = Source.fromFile("Files/Day1.txt")
  val in = src.getLines.next.split(", ")
  src.close()
  val regex = """([LR])(\d+)""".r

  case class PointWithDir(x: Long, y: Long, dir: Double) {
    def distanceTo(other: PointWithDir) = Math.abs(x - other.x) + Math.abs(y - other.y)
  }

  def getNewDir(turn: String, prev: PointWithDir): PointWithDir = {
    val nDir = prev.dir + {
      if (turn == "L") .5 else -.5
    }
    PointWithDir(Math.round(Math.cos(nDir * Math.PI)), Math.round(Math.sin(nDir * Math.PI)), nDir)
  }

  val starting = PointWithDir(0, 0, .5)

  //PART 1
  val endingPoint = in.foldLeft(starting) { (c, s) =>
    val regex(dir, stpStr) = s
    val steps = stpStr.toInt
    val nDir = getNewDir(dir, c)
    c.copy(x = c.x + (nDir.x * steps), y = c.y + (nDir.y * steps), nDir.dir)
  }

  println(s"Part 1: ${endingPoint.distanceTo(starting)}")

  //PART 2
  val pt2List = starting :: in.scanLeft(starting) { (c, s) =>
    val regex(dir, stpStr) = s
    val steps = stpStr.toInt
    val nDir = getNewDir(dir, c)
    c.copy(x = c.x + (nDir.x * steps), y = c.y + (nDir.y * steps), nDir.dir)
  }.sliding(2).flatMap { case Array(p1, p2) => (for (x <- (p1.x to p2.x by (if (p2.x - p1.x < 0) -1 else 1)); y <- (p1.y to p2.y by (if (p2.y - p1.y < 0) -1 else 1))) yield {
    PointWithDir(x, y, p1.dir)
  }).tail
  }.toList

  @tailrec
  def getFirstDuplicate(list: Seq[PointWithDir]): Option[PointWithDir] = list match {
    case head :: tail if (tail.exists(p => head.x == p.x && head.y == p.y)) => Some(PointWithDir(head.x, head.y, 0))
    case head :: tail => getFirstDuplicate(tail)
    case head :: Nil => None
  }

  getFirstDuplicate(pt2List) match {
    case Some(p) => println(s"Part 2: ${p.distanceTo(starting)}")
    case _ =>
  }
}
