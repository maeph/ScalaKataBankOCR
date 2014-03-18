package pl.allegro.funkata

import scala.io.{BufferedSource, Source}

object OCRReader {

  type StringLine = List[String]

  def readFile(fileName: String): List[String] = {
    val file: BufferedSource = Source.fromURL(getClass.getResource(fileName))
    val input: List[String] = file.getLines().toList
    file.close()
    input
  }
}

