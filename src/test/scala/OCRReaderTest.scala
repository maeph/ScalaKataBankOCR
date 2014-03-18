package pl.allegro.funkata

import org.scalatest.{BeforeAndAfter, FunSpec, GivenWhenThen}

class OCRReaderTest extends FunSpec with GivenWhenThen with BeforeAndAfter {

  describe("OCR Reader") {
    val expectedOutput = "Hello Scala!"
    Given("starter file")
    When("reader reads the file")
    val output = OCRReader.readFile("/starter.txt").head
    Then(s"output should be $expectedOutput")
    assert(output == expectedOutput)
  }
  
}
