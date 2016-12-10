import scala.io.Source

/**
  * Created by Montana Ruth on 12/6/2016.
  */

object Day7 extends App {
  val abbaR = """(.)(?!\1)(.)\2\1""".r
  val abaR = """(.)(?!\1)(.)\1""".r
  val hypernetR = """\[(.+?)\]""".r
  val src = Source.fromFile("Files/Day7.txt")
  val in = src.getLines().toList
  src.close()

  def supportsTLS(str: String): Boolean = {
    if(hypernetR.findAllMatchIn(str).flatMap(_.group(1).sliding(4)).exists(abbaR.findFirstIn(_).isDefined))
      false
    else
      hypernetR.split(str).flatMap(_.sliding(4)).exists(abbaR.findFirstIn(_).isDefined)
  }

  def supportsSSL(str: String) = {
    val superSliding = hypernetR.split(str).flatMap(_.sliding(3)).toSet
    val hyperSliding = hypernetR.findAllMatchIn(str).flatMap(_.group(1).sliding(3)).toSet

    superSliding.exists(abaR.findFirstIn(_) match {
      case Some(aba) => hyperSliding.exists(_ == aba.tail+aba.tail.head)
      case None => false
    })
  }

  def part1: Unit = {
    val ans = in.count{supportsTLS}
    println(s"Part1: $ans")
  }

  def part2: Unit = {
    val ans = in.count(supportsSSL)
    println(s"Part2: $ans")
  }

  part1
  part2
}