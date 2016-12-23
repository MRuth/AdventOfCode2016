
/**
  * Created by Montana Ruth on 12/23/2016.
  */

import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success, Try}
object Day23 extends App {
  val src = Source.fromFile("Files/Day23.txt")
  val inst = parseInstructions(src.getLines().toList)
  src.close()

  trait Instruction
  case class cpy(x: Either[Char,Int], y: Either[Char,Int]) extends Instruction {
    def exec(registers: mutable.Map[Char,Int]) = if(y.isLeft)
      registers(y.left.get) = x match {
      case Right(x) => x
      case Left(r) => registers(r)
    }
  }
  case class inc(x: Char) extends Instruction {
    def exec(registers: mutable.Map[Char,Int]) = registers(x) += 1
  }
  case class dec(x: Char) extends Instruction {
    def exec(registers: mutable.Map[Char,Int]) = registers(x) -= 1
  }
  case class jnz(x: Either[Char,Int], y: Either[Char,Int]) extends Instruction
  {
    def exec(registers: mutable.Map[Char,Int]) = x match {
      case Right(x) if (x != 0) => if(y.isRight) y.right.get else registers(y.left.get)
      case Left(x) if(registers(x) != 0) => if(y.isRight) y.right.get else registers(y.left.get)
      case _ => 1
    }
  }
  case class tgl(x: Char) extends Instruction{
    def exec(ctr: Int, registers: mutable.Map[Char,Int],instructions: mutable.Buffer[Instruction]) =
      if(instructions.isDefinedAt(ctr+registers(x)))
        instructions(ctr+registers(x)) = instructions(ctr+registers(x)) match {
          case i: inc => dec(i.x)
          case i: dec => inc(i.x)
          case i: tgl => inc(i.x)
          case i: jnz => cpy(i.x,i.y)
          case i: cpy => jnz(i.x,i.y)
        }
  }

  def parseInstructions(strInst: List[String]): List[Instruction] = {
    def toEither(str: String): Either[Char, Int] = Try(str.toInt) match {
      case Success(int) => Right(int)
      case Failure(_) => Left(str.head)
    }

    strInst.map { str =>
      val split = str.split(" ")
      split(0) match {
        case "cpy" => cpy(toEither(split(1)), toEither(split(2)))
        case "inc" => inc(split(1).head)
        case "dec" => dec(split(1).head)
        case "jnz" => jnz(toEither(split(1)), toEither(split(2)))
        case "tgl" => tgl(split(1).head)
      }
    }
  }

  def eval(registers: mutable.Map[Char,Int]): Int = {
    val instructions = inst.toBuffer
    var ctr = 0
    while(instructions.isDefinedAt(ctr)){
      //println(ctr)
      instructions(ctr) match {
        case i: cpy => i.exec(registers);ctr+=1
        case i: inc => i.exec(registers);ctr+=1
        case i: dec => i.exec(registers);ctr+=1
        case i: jnz =>
          //if(i.y.isRight && i.y.right.get == -2)
          //println("TEST")
          ctr += i.exec(registers)
        case i: tgl => i.exec(ctr,registers,instructions);ctr+=1
      }
    }
    registers('a')
  }

  def part1: Unit = {
    val registers =  mutable.Map[Char,Int]('a' -> 7,'b' -> 0,'c' -> 0,'d' -> 0)
    println(s"Part 1: ${eval(registers)}")
  }
  def part2: Unit = {
    val registers =  mutable.Map[Char,Int]('a' -> 12,'b' -> 0,'c' -> 0,'d' -> 0)
    println(s"Part 2: ${eval(registers)}")
  }

  part1
  part2
}