
/**
  * Created by Montana Ruth on 12/4/2016.
  */

object Day5 extends App {
  val md5: String => String = (text) => org.apache.commons.codec.digest.DigestUtils.md5Hex(text)
  val roomId = "abbhdwsy"

  def part1(): Unit = {
    val key = Stream.continually(roomId).zipWithIndex.map{case (id,idx) => md5(id+idx)}.
      filter(_.startsWith("00000")).map(_(5)).take(8).mkString

    println(key)
  }

  def part2(): Unit = {
    val availKeyIndexes = collection.mutable.Set(('0' to '7') :_*)
    val key = Stream.continually(roomId).zipWithIndex.map{case (id,idx) => md5(id+idx)}.
      filter(s => s.startsWith("00000") && availKeyIndexes.remove(s(5))).map(s => ((s(5)-48),s(6))).
      take(8).sortBy(_._1).map(_._2).mkString

    println(key)
  }

  part1()
  part2()
}