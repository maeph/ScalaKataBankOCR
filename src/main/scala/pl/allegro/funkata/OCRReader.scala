package pl.allegro.funkata
import scala.io.{BufferedSource, Source}
import java.net.URI

class OCRReader {

  def readInput(fileName: String): String = {
    val chars: List[List[List[Char]]] = readFile(fileName) map {splitToThrees(_)}

    (chars(0) zip chars(1) zip chars(2) zip chars(3)) map {
      case (((a, b), c), d) => 
        List(a mkString, b mkString, c mkString, d mkString) mkString (sys.props("line.separator"))
    } map {
      case Digits.ZERO => "0"
      case Digits.ONE => "1"
      case Digits.TWO => "2"
      case _ => "E"
    } mkString



}


  def readFile(fileName: String): List[String] = {
    val file: BufferedSource = Source.fromURL(getClass.getResource(fileName))
    val input: List[String] = file.getLines().toList
    file.close()
    input
  }

  def splitToThrees (text: String, acc: List[List[Char]] = List.empty[List[Char]]): List[List[Char]] = {
    text match {
      case "" => acc
      case s => splitToThrees(s.drop(3), acc :+ s.take(3).toCharArray.toList)

   }
  }


}
