import scala.annotation.tailrec
import scala.collection._
import scala.collection.immutable.Queue

/**
  * Created by Montana Ruth on 12/17/2016.
  */

object Day17 extends App {
  val maze = "##########S| | | ##-#-#-#-## | | | ##-#-#-#-## | | | ##-#-#-#-## | | |  ####### V".grouped(9).
    map(_.toCharArray).toArray
  val in = "lpvhkcbi"
  val openChrs = Set('b','c','d','e','f')
  val passcode: String => String = path => org.apache.commons.codec.digest.DigestUtils.md5Hex(in+path).take(4)
  case class Move(x: Int, y: Int, path: String)
  def isValidMove(x: Int, y:Int) = (maze.isDefinedAt(y) && maze(y).isDefinedAt(x) && maze(y)(x) != '#')

  def recurSolve(): (Option[String],Int) = {
    @tailrec
    def recur(moves: Queue[Move], shortestPath: Option[String], longestPath: Int): (Option[String], Int) =
      moves match {
        case Queue() => return (shortestPath, longestPath)
        case head +: tail if (isValidMove(head.x + 1, head.y + 1) && maze(head.y + 1)(head.x + 1) == 'V') =>
          recur(tail, if (shortestPath == None) Some(head.path) else shortestPath,
            longestPath.max(head.path.length))
        case head +: tail => recur(getNewTail(head, tail), shortestPath, longestPath)
      }
    def getNewTail(head: Move, tail: Queue[Move]) = {
      val pass = passcode(head.path)
      tail.enqueue(List(
        if (isValidMove(head.x, head.y - 2) && openChrs(pass(0))) Some(Move(head.x, head.y - 2, head.path + "U")) else None,
        if (isValidMove(head.x, head.y + 2) && openChrs(pass(1))) Some(Move(head.x, head.y + 2, head.path + "D")) else None,
        if (isValidMove(head.x - 2, head.y) && openChrs(pass(2))) Some(Move(head.x - 2, head.y, head.path + "L")) else None,
        if (isValidMove(head.x + 2, head.y) && openChrs(pass(3))) Some(Move(head.x + 2, head.y, head.path + "R")) else None
      ).flatten)
    }
    recur(Queue[Move](Move(1,1,"")),None,-1)
  }

  val (part1,part2) = recurSolve()
  println(s"Part 1: ${part1.get}")
  println(s"Part 2: $part2")
}