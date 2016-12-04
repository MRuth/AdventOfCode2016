import scala.io.Source

/**
  * Created by Montana Ruth on 12/3/2016.
  */

object Day4 extends App {
  case class Room(name: String, id: Int, checksum: String)
  val src = Source.fromFile("Files/Day4.txt")
  val regex = """(.+?)-(\d+)\[(.+)\]""".r
  val in = src.getLines().map{s => val regex(name,id,checksum) = s; Room(name,id.toInt,checksum)}.toList
  src.close()

  def part1(): Unit = {
    val ans = in.foldLeft(0){(c,room) =>
      val common5 = room.name.filterNot(_ == '-').groupBy(identity).toSeq.
        sortBy{case (chr,grp) => (-grp.length,chr)}.map(_._1).take(5).mkString
      if(common5 == room.checksum) c+room.id else c
    }

    println(s"Part 1: $ans")
  }

  def part2(): Unit = {
    def decrypt(str: String,shift: Int):String =
      str.map(c => if(c == '-')' ' else ((((c-'a')+shift)%26)+'a').toChar)

    val ans = in.find{room => decrypt(room.name,room.id).startsWith("north")}
      .collect{case room => room.id.toString}.getOrElse("NOT FOUND")

    println(s"Part 2: $ans")
  }

  part1()
  part2()
}