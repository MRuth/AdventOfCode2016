import scala.io.Source

/**
  * Created by Montana Ruth on 12/1/2016.
  */

object Day2 extends App {
  case class Point(x: Int, y:Int)
  val src = Source.fromFile("Files/Day2.txt")
  val in = src.getLines.toList
  src.close()

  val getCombination: List[List[Char]] => String = (keyPad) =>
    (in.scanLeft(Point(1,1)){(prevPoint,str) => str.foldLeft(prevPoint)(moveToKey(_,_,keyPad))}).
      tail.map(p => keyPad(p.y)(p.x)).mkString

  def part1(): Unit = {
    val keyPad = List(List('1','2','3'),List('4','5','6'),List('7','8','9'))

    println(s"Part 2: ${getCombination(keyPad)}")
  }

  def part2(): Unit = {
    val keyPad = List(List('_','_','1','_','_'),List('_','2','3','4','_'),
      List('5','6','7','8','9'),List('_','A','B','C','_'), List('_','_','D','_','_'))

    println(s"Part 1: ${getCombination(keyPad)}")
  }

  def moveToKey(prev: Point, dir: Char, keypad:List[List[Char]]):Point = dir match {
    case 'L' if(prev.x-1 >= 0 && keypad(prev.y)(prev.x-1) != '_') =>
      prev.copy(x=prev.x-1)
    case 'R' if(prev.x+1 < keypad(prev.y).length && keypad(prev.y)(prev.x+1) != '_') =>
      prev.copy(x=prev.x+1)
    case 'U' if(prev.y-1 >= 0 && keypad(prev.y-1)(prev.x) != '_') =>
      prev.copy(y=prev.y-1)
    case 'D' if(prev.y+1 < keypad.length && keypad(prev.y+1)(prev.x) != '_') =>
      prev.copy(y=prev.y+1)
    case  _ =>
      prev
  }

  part1()
  part2()
}