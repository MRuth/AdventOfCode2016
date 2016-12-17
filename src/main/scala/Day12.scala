import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * Created by Montana Ruth on 12/11/2016.
  */

object Day12 extends App {
  val cpyR = """cpy (.+?) (.+?)""".r
  val incR = """inc (.)""".r
  val decR = """dec (.)""".r
  val jnzR = """jnz (.+?) (.+?)""".r

  def toEither(x: String): Either[Char, Int] =
    Try(x.toInt) match {
      case Success(n) => Right(n)
      case Failure(_) => Left(x.head)
    }

  val src = Source.fromFile("Files/Day12.txt")
  val instructions = src.getLines.toArray
  src.close()

  def eval(initRegisters: Seq[(Char,Int)]): Int = {
    val registers = mutable.HashMap(initRegisters : _*)
    def cpy(x: Either[Char, Int], y: Char): Unit = x match {
      case Right(v) => registers(y) = v
      case Left(r) => registers(y) = registers(r)
    }

    def inc(x: Char): Unit = registers(x) += 1
    def dec(x: Char): Unit = registers(x) -= 1
    def jnz(x: Either[Char, Int], y: Int): Int = x match {
      case Right(v) => if (v != 0) y else 1
      case Left(c) => if (registers(c) != 0) y else 1
    }
    var idx = 0
    while (idx >= 0 && idx < instructions.length) {
      instructions(idx) match {
        case cpyR(x, y) => cpy(toEither(x), y.head); idx += 1
        case incR(x) => inc(x.head); idx += 1
        case decR(x) => dec(x.head); idx += 1
        case jnzR(x, y) => idx += jnz(toEither(x), y.toInt)
      }
    }
    registers('a')
  }

  def part1: Unit = {
    println(s"Part 1: ${eval(Seq(('a',0),('b',0),('c',0),('d',0)))}")
  }
  def part2: Unit = {
    println(s"Part 2: ${eval(Seq(('a',0),('b',0),('c',1),('d',0)))}")
  }

  part1
  part2
}