import scala.io.Source

/**
  * Created by Montana Ruth on 12/21/2016.
  */

object Day21 extends App {
  val src = Source.fromFile("Files/Day21.txt")
  val inst = parseInst(src.getLines())
  src.close()

  trait Direction
  case object Left extends Direction
  case object Right extends Direction
  def swapPositions(pos1: Int,pos2: Int)(str: String) = {
    val tmp = str.charAt(pos2)
    str.updated(pos2,str.charAt(pos1)).updated(pos1,tmp)
  }
  def swapLetters(l1: Char, l2: Char)(str: String) = swapPositions(str.indexOf(l1),str.indexOf(l2))(str)
  def rotate(rot: Int, dir: Direction)(str: String) = {
    val take = if(dir == Right) str.length - (rot % str.length) else (rot % str.length)
    val (p1,p2) = str.splitAt(take)
    p2+p1
  }
  def rotateLeft(rot: Int)(str: String) = rotate(rot, Left)(str)
  def rotateRight(rot: Int)(str: String) = rotate(rot, Right)(str)
  def rotateOnLetter(char: Char)(str: String) = str.indexOf(char) match {
    case x if (x >= 4) => rotateRight(x+2)(str)
    case x => rotateRight(x+1)(str)
  }
  def reversePos(from: Int, to: Int)(str: String) = {
    val nw = str.slice(from,to+1).reverse
    str.patch(from,nw,nw.length)
  }
  def move(from: Int, to: Int)(str: String) = {
    val (rest,chr) = str.partition(_ != str.charAt(from))
    new StringBuilder(rest).insert(to,chr).toString
  }

  def parseInst(strInst: Iterator[String]): List[String => String] = {
    val swapPosR  = """swap position (\d+) .+ (\d+)""".r
    val swapLtrR  = """swap letter (.) .+? (.)""".r
    val rotateR   = """rotate (left|right) (\d+) .+""".r
    val rotateOnR = """rotate based .+ (.)""".r
    val reverseR  = """reverse .+ (\d+) .+ (\d+)""".r
    val moveR     = """move .+ (\d+) .+ (\d+)""".r

    strInst.collect{
        case swapPosR(from,to) => swapPositions(from.toInt,to.toInt)(_)
        case swapLtrR(from,to) => swapLetters(from.head,to.head)(_)
        case rotateR(dir,rot) if(dir == "left") =>rotateLeft(rot.toInt)(_)
        case rotateR(dir,rot) if(dir == "right") =>rotateRight(rot.toInt)(_)
        case rotateOnR(chr) => rotateOnLetter(chr.head)(_)
        case reverseR(from,to) => reversePos(from.toInt,to.toInt)(_)
        case moveR(from,to) => move(from.toInt,to.toInt)(_)
    }.toList
  }

  def eval(s: String): String = inst.foldLeft(s)((str,f)=> f(str))
  def part1: Unit = println(s"Part 1: ${eval("abcdefgh")}")
  def part2: Unit = println(s"Part 2: ${"fbgdceah".permutations.find(eval(_) == "fbgdceah").get}")

  part1
  part2
}