package pl.allegro.funkata

import org.scalatest.{BeforeAndAfter, FunSpec, GivenWhenThen}

class OCRReaderTest extends FunSpec with GivenWhenThen with BeforeAndAfter{
  var readerUnderTest: OCRReader = _
  before {
    readerUnderTest = new OCRReader
  }
  
  def checkThat(thenString: String, expectedOutput: String, fileName: String ): Unit = {
    Given(s"file name with $expectedOutput")
    When("reader reads file")
    val output = readerUnderTest.readInput(fileName)
    Then(thenString)
    assert (output == expectedOutput)
  }

  describe("OCR Reader") {
    it("should read different digits file") {
      checkThat("output should be 123456789", "123456789", "/test11.txt")
    }
    it("should validate correctness of digits") {
      checkThat("output should contain illegal message", "49006771? ILL", "/test_ill.txt")
    }
    
    it("should validate crc sum of the series of digits") {    
      checkThat("output should be crc checked", "111111111 ERR", "/test2.txt")
    }
  
    



  }
}
