package pl.allegro.funkata
import scala.io.{BufferedSource, Source}
import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq

object OCRReader {

  type StringLine = List[String]


  def readInput(fileName: String): String = {
    val digitals: List[StringLine] = readFile(fileName) map {splitToStringLine(_)}
    val output = parseInputDigitals(digitals)
    invalidate(output)
  }


  def parseInputDigitals(lines: List[StringLine]): String =
    (lines(0) zip lines(1) zip lines(2) zip lines(3)).map {
      case (((a, b), c), d) => List(a, b, c, d).mkString(sys.props("line.separator"))
    }.map(Digits.digitalToDigit).mkString


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

  @tailrec
  private[this] def splitToStringLine (text: String, acc: StringLine = List()): StringLine =
    text match {
      case "" => acc
      case s => splitToStringLine(s.drop(3), acc :+ s.take(3))
    }

  def switchedSingleChar(input: String, position: Int): Set[String] = {
    val chars:Set[Char] = Set(' ', '_', '|')
    chars.map(input.take(position) + _ + input.drop(position + 1))
  }
  def switchedSingleCharCombination(input: String) : Set[String] = {
    val morphedCombinations: IndexedSeq[Set[String]] = for {
      i <- 0 until input.size
    } yield {
      switchedSingleChar(input, i)
    }
    morphedCombinations.toSet.flatten
  }
}

