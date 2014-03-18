package pl.allegro.funkata
import scala.io.{BufferedSource, Source}
import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq
import javax.management.remote.rmi._RMIConnection_Stub

object OCRReader {

  def generateFuzzyReads(lines: List[String]): Set[List[String]] = {
    val line0Morphed: Set[List[String]] = switchedSingleCharCombination(lines(0)) map {
      case line => line :: lines.tail
    }
    val line1Morphed: Set[List[String]] = switchedSingleCharCombination(lines(1)) map {
      case line => List(lines(0),line, lines(2),lines(3))
    }
    val line2Morphed: Set[List[String]] = switchedSingleCharCombination(lines(2)) map {
      case line => List(lines(0),lines(1), line, lines(3))
    } 
    
     line0Morphed ++ line1Morphed ++ line2Morphed
  }

  def readFuzzyInput(fileName: String): String = {
    val fuzzyReads:Set[List[String]] = generateFuzzyReads(readFile(fileName))
    val readDigits: Set[String] = fuzzyReads map readInputLines
    val correctDigits: Set[String] = readDigits filter {case r => !(r.contains("ILL") || r.contains("ERR"))}
    correctDigits.head
    
  }

  type StringLine = List[String]


  def readInput(fileName: String): String = {
    val lines: List[String] = readFile(fileName)
    readInputLines(lines)
  }


  def readInputLines(lines: List[String]): String = {
    val digitals: List[StringLine] = lines map {
      splitToStringLine(_)
    }
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

