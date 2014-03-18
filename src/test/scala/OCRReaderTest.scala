package pl.allegro.funkata

import org.scalatest.{BeforeAndAfter, FunSpec, GivenWhenThen}

class OCRReaderTest extends FunSpec with GivenWhenThen with BeforeAndAfter{

  def checkThat(readFn:(String) => String, thenString: String, expectedOutput: String, fileName: String ): Unit = {
    Given(s"file name with $expectedOutput")
    When("reader reads file")
    val output = readFn(fileName)
    Then(thenString)
    assert (output == expectedOutput)
  }


  def checkThatContains(readFn:(String) => String,thenString: String, expectedOutput: List[String], fileName: String ): Unit = {
    Given(s"file name with $expectedOutput")
    When("reader reads file")
    val output = readFn(fileName)
    Then(thenString)
    assert (expectedOutput.contains(output))
  }

  describe("OCR Reader") {
    it("should read different digits file") {
      checkThat(OCRReader.readFuzzyInput, "output should be 123456789", "123456789", "/test11.txt")
    }
    it("should validate correctness of digits") {
      checkThat(OCRReader.readFuzzyInput, "output should contain illegal message", "49006771? ILL", "/test_ill.txt")
    }
    
    it("should validate crc sum of the series of digits") {    
      checkThat(OCRReader.readInput, "output should be crc checked", "111111111 ERR", "/test2.txt")
    }

    it("should provide closest correct output for 111111111") {
      checkThat(OCRReader.readFuzzyInput, "output should be closest correct digits", "711111111", "/test2.txt")
    }
    
    it("should provide closest correct output for 555555555") {
      checkThatContains(OCRReader.readFuzzyInput, "output should be list of closest correct digits", 
        List("555555555 AMB ['555655555', '559555555']","555555555 AMB ['559555555', '555655555']"), "/test6.txt")
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
