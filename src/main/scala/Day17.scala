import java.util

import scala.annotation.tailrec
import scala.collection.mutable

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

  def solve(): (String,String) = {
    val queue = mutable.Queue[Move](Move(1,1,""))
    var shortestPath: Option[String] = None
    var longestPath: Int = -1
    while (queue.nonEmpty) {
      val head = queue.dequeue()
      if(isValidMove(head.x+1,head.y+1) && maze(head.y+1)(head.x+1) == 'V') {
        if(shortestPath == None) shortestPath = Some(head.path)
        longestPath = longestPath.max(head.path.length)
      }
      else {
        val pass = passcode(head.path)
        if (isValidMove(head.x, head.y - 2) && openChrs(pass(0))) queue.enqueue(Move(head.x,head.y-2,head.path+"U"))
        if (isValidMove(head.x, head.y + 2) && openChrs(pass(1))) queue.enqueue(Move(head.x,head.y+2,head.path+"D"))
        if (isValidMove(head.x - 2, head.y) && openChrs(pass(2))) queue.enqueue(Move(head.x-2,head.y,head.path+"L"))
        if (isValidMove(head.x + 2, head.y) && openChrs(pass(3))) queue.enqueue(Move(head.x+2,head.y,head.path+"R"))
      }
    }
    return (shortestPath.get,longestPath.toString)
  }

  val (part1,part2) = solve()
  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
}