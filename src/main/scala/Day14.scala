
/**
  * Created by Montana Ruth on 12/14/2016.
  */

object Day14 extends App {

  val md5Hash: String => String = (in) => org.apache.commons.codec.digest.DigestUtils.md5Hex(in)
  val threeSameR = """(.)\1\1""".r
  val salt = "ngcjuoqr"
  val otpStream = Stream.continually(salt).zip(Stream.iterate(1L){c => c+1}).
    map{case (salt,n) => (n,md5Hash(salt+n))}

  val get64thPad: Stream[(Long,String)] => Long = (hashStream) => hashStream.sliding(1001).map(_.force).filter{
    case head #:: tail =>
      threeSameR.findFirstMatchIn(head._2) match {
        case Some(c3) => tail.exists(_._2.contains(c3.group(1)*5))
        case _ => false
      }
  }.take(64).toList.last.head._1

  def part1: Unit = {
    println(s"Part 1: ${get64thPad(otpStream)}")
  }
  def part2: Unit = {
    println(s"Part 2: ${get64thPad(otpStream.map(h1 => (h1._1,Iterator.iterate(h1._2)(md5Hash).drop(2016).next)))}")
  }

  part1
  part2
}