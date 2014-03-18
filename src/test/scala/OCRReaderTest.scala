package pl.allegro.funkata

import org.scalatest.{BeforeAndAfter, FunSpec, GivenWhenThen}

class OCRReaderTest extends FunSpec with GivenWhenThen with BeforeAndAfter{
  var readerUnderTest: OCRReader = _
  before {
    readerUnderTest = new OCRReader
  }
  
  def checkThat(readFn:(String) => Any,thenString: String, expectedOutput: Any, fileName: String ): Unit = {
    Given(s"file name with $expectedOutput")
    When("reader reads file")
    val output = readFn(fileName)
    Then(thenString)
    assert (output == expectedOutput)
  }

  describe("OCR Reader") {
    it("should read different digits file") {
      checkThat(readerUnderTest.readInput, "output should be 123456789", "123456789", "/test11.txt")
    }
    it("should validate correctness of digits") {
      checkThat(readerUnderTest.readInput, "output should contain illegal message", "49006771? ILL", "/test_ill.txt")
    }
    
    it("should validate crc sum of the series of digits") {    
      checkThat(readerUnderTest.readInput, "output should be crc checked", "111111111 ERR", "/test2.txt")
    }

  }
  
  describe("singleSwitched") {
    it("should morph middle char") {
      Given("String \" _ \"")
        val input = " _ "
      When("produces morphed strings")
        val output: Set[String] = OCRReader.switchedSingleChar(input, 1)
      Then("should contain all strings")
      assert(output == Set("   ", " | ", " _ "))
    }
    it("should morph last char") {
      Given("String \" _ \"")
      val input = " _ "
      When("produces morphed strings")
      val output: Set[String] = OCRReader.switchedSingleChar(input, 2)
      Then("should contain all strings")
      assert(output == Set(" __", " _ ", " _|"))
    }
    
    it("should produce every combination") {
      Given("String \" _ \"")
      val input = " _ "
      When("produces morphed strings")
      val output: Set[String] = OCRReader.switchedSingleCharCombination(input)
      Then("should contain all strings")
      assert(output == Set("__ ", "|_ ","   ", " | ", " _ ", " __", " _|"))

    }
  }
  
}
