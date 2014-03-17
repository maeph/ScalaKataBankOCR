package pl.allegro.funkata
import scala.io.{BufferedSource, Source}
import java.net.URI

class OCRReader {
  
  type CharLine = List[Char]

  def readInput(fileName: String): String = {
    val chars: List[List[CharLine]] = readFile(fileName) map {splitToCharLines(_)}

    val output = 
    (chars(0) zip chars(1) zip chars(2) zip chars(3)) map {
      case (((a, b), c), d) => 
        List(a, b, c, d) map (_.mkString) mkString (sys.props("line.separator"))
    } map digitalToDigit mkString  
    
    invalidate(output) 
  }


  def invalidate (output: String):String = output match {
      case invalid if invalid.contains("?") => invalid + " ILL"
      case s => s
    }
  

  def digitalToDigit: (String) => String = {

    case Digits.ZERO => "0"
    case Digits.ONE => "1"
    case Digits.TWO => "2"
    case Digits.THREE => "3"
    case Digits.FOUR => "4"
    case Digits.FIVE => "5"
    case Digits.SIX => "6"
    case Digits.SEVEN => "7"
    case Digits.EIGHT => "8"
    case Digits.NINE => "9"
    case _ => "?"

  }

  def readFile(fileName: String): List[String] = {
    val file: BufferedSource = Source.fromURL(getClass.getResource(fileName))
    val input: List[String] = file.getLines().toList
    file.close()
    input
  }

  def splitToCharLines (text: String, acc: List[CharLine] = List.empty[CharLine]): List[CharLine] =
    text match {
      case "" => acc
      case s => splitToCharLines(s.drop(3), acc :+ s.take(3).toCharArray.toList)
     }
  


}
