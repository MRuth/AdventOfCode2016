import scala.collection.mutable
import scala.io.Source

/**
  * Created by Montana Ruth on 12/9/2016.
  */

object Day10 extends App {
  trait hasInv{
    val inv = mutable.SortedSet[Int]()
  }

  case class Bot(id: Int) extends hasInv{
    var lowTo: hasInv = _
    var highTo: hasInv = _
  }
  case class Output(id: Int) extends hasInv

  val botAssignR = """^bot (\d+) gives .+ (.+?) (\d+) .+ (.+?) (\d+)""".r
  val chipAssignR = """value (\d+) goes to bot (\d+)""".r
  val src = Source.fromFile("Files/Day10.txt")
  val in = src.getLines().toList
  src. close()
  val bots = (0 to 209).map(Bot)
  val outputs = (0 to 20).map(Output)
  val activeChips = collection.mutable.Set[Int]()

  def giveChip(chip: Int, to: hasInv): Unit = {
    to match {
      case bot: Bot =>bot.inv += chip
      case output: Output => output.inv += chip; activeChips.remove(chip)
    }
  }
  in.foreach{pass =>
    pass match{
      case botAssignR(fromId, lowTo, lowId, highTo, highId) =>{
        bots(fromId.toInt).lowTo = if(lowTo == "bot") bots(lowId.toInt) else outputs(lowId.toInt)
        bots(fromId.toInt).highTo = if(highTo == "bot") bots(highId.toInt) else outputs(highId.toInt)
      }
      case chipAssignR(chipId, toId) => bots(toId.toInt).inv+= chipId.toInt;activeChips += chipId.toInt
    }
  }


  while(activeChips.nonEmpty){
    bots.find{_.inv.size >= 2}.collect{case bot =>
      if(bot.inv(61) && bot.inv(17))
        println(s"Part 1: ${bot.id}")
      val low = bot.inv.head
      val high = bot.inv.last
      giveChip(low,bot.lowTo)
      giveChip(high,bot.highTo)
      bot.inv.remove(low)
      bot.inv.remove(high)
    }
  }

  println(s"Part 2: ${outputs(0).inv.head*outputs(1).inv.head*outputs(2).inv.head}")
}