import java.nio.channels.Pipe.SourceChannel

import scala.io.Source

/**
  * Created by Montana Ruth on 12/8/2016.
  */

object Day9 extends App {
  val markerR = """(?:\((\d+)x(\d+)\))""".r
  val src = Source.fromFile("Files/Day9.txt")
  val in = src.getLines().next
  src.close()

  def decompress(s: String, recursive: Boolean): Long = {
    if (s.length == 0) return 0
    markerR.findFirstMatchIn(s) match {
      case None => s.length
      case Some(marker) => {
        val adCt1 = marker.start
        val len = marker.group(1).toInt
        val mult = marker.group(2).toInt
        val adCt2 = {
          if (recursive) {
            val innerCount = decompress(s.substring(marker.end, marker.end + len), recursive)
            (innerCount * mult)
          }
          else
            len * mult
        }
        val text = s.substring(marker.end + len)
        val adCt3 = (decompress(text, recursive))
        adCt1 + adCt2 + adCt3
      }
    }
  }

  println(s"Part1: ${decompress(in, false)}")
  println(s"Part2: ${decompress(in, true)}")
}