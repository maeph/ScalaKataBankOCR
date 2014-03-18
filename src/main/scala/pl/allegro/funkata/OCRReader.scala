package pl.allegro.funkata
import scala.io.{BufferedSource, Source}
import java.net.URI
import scala.collection.immutable.IndexedSeq

class OCRReader {
  
  type CharLine = List[Char]
  

  def readInput(fileName: String): String = {
    val digitals: List[List[CharLine]] = readFile(fileName) map {splitToCharLines(_)}
    val output = parseInputDigitals(digitals)  
    invalidate(output) 
  }


  def parseInputDigitals(chars: List[List[CharLine]]): String = {
    (chars(0) zip chars(1) zip chars(2) zip chars(3)) map {
      case (((a, b), c), d) =>
        List(a, b, c, d) map (_.mkString) mkString (sys.props("line.separator"))
    } map Digits.digitalToDigit mkString
  }

   

  def crCheck(output: String): Boolean = {
    val crcSum: Int = output.map(_.asDigit).reverse.zipWithIndex.map {
      case (x, index) =>  x * (index + 1)
    }.sum
    crcSum % 11 == 0   
  }
  
  def invalidate (output: String):String = output match {
      case invalid if invalid.contains("?") => invalid + " ILL"
      case errornous if !crCheck(output) => errornous + " ERR"
      case s => s
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


object OCRReader {
  val chars:Set[Char] = Set(' ', '_', '|')
  def switchedSingleChar(input: String, position: Int): Set[String] = {
    chars.map(input.take(position) + _ + input.drop(position + 1))   
  }
  def fuzzyMatch(digit: String): Set[String] = {
    val closeDigitals: IndexedSeq[String] = for {
      i <- 0 until digit.size
      potentialDigit <- switchedSingleChar(digit, i)
    } yield {
        Digits.digitalToDigit(potentialDigit)
      } 
    
    closeDigitals.filter(_ != "?").toSet
  }
}