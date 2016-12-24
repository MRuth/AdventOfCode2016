import scala.collection.mutable.Queue
import scala.io.Source
/**
  * Created by Montana Ruth on 12/24/2016.
  */

object Day24 extends App {
  case class Point(y: Int, x: Int, parent: Option[Point],dist: Int)
  val src = Source.fromFile("Files/Day24.txt")
  val maze = src.getLines().map(_.toArray).toArray
  src.close()
  val points = maze.zipWithIndex.map(t => (t._2,t._1.zipWithIndex.filter(_._1.isDigit).toList)).
    filter(_._2.nonEmpty).flatMap{case (y,list) => list.map{case (c,x) => c.asDigit -> Point(y,x,None,0)}}.toMap
  val pairPoints = points.keys.toSeq.combinations(2).map{case Seq(p1,p2) => if(p1 < p2)(p1,p2) else (p2,p1)}.toList
  val pairDists = pairPoints.map{case (p1,p2) => (p1,p2) -> findSolution(maze,points(p1),points(p2))}.toMap

  def findSolution(maze: Array[Array[Char]], startPoint: Point, endPoint: Point): Int = {
    val locMaze = maze.map(_.toBuffer).toBuffer
    val path = Queue[Point](startPoint)
    def getNewPoint(prev: Point, yOff: Int, xOff: Int):Option[Point] = {
      val y = prev.y+yOff
      val x = prev.x+xOff
      if (locMaze.isDefinedAt(y) && locMaze(y).isDefinedAt(x) && locMaze(y)(x) != '#')
        Some(Point(y,x,Some(prev),prev.dist+1))
      else
        None
    }

    while(path.nonEmpty) {
      val curr = path.dequeue()
      if(curr.y == endPoint.y && curr.x == endPoint.x)
        return curr.dist
      else
        List(getNewPoint(curr,-1,0),getNewPoint(curr,1,0),getNewPoint(curr,0,-1),getNewPoint(curr,0,1)).flatten.
          foreach{p => locMaze(p.y)(p.x) = '#';path.enqueue(p)}
    }
    return Int.MaxValue
  }

  def part1: Unit =
    println(s"Part 1: ${
      points.keys.toSeq.permutations.filter(_.head == 0).map{perm =>
        perm.sliding(2).map{case Seq(p1,p2) =>
          if (p1 < p2) pairDists((p1, p2)) else pairDists(p2, p1)
        }.sum
      }.min
    }")

  def part2: Unit =
    println(s"Part 1: ${
      points.keys.toSeq.permutations.filter(_.head == 0).map{perm =>
        (perm :+ 0).sliding(2).map{case Seq(p1,p2) =>
          if (p1 < p2) pairDists((p1, p2)) else pairDists(p2, p1)
        }.sum
      }.min
    }")

  part1
  part2
}