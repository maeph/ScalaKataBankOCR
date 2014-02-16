package pl.allegro.funkata

import org.scalatest.{BeforeAndAfter, FunSpec, GivenWhenThen}

class OCRReaderTest extends FunSpec with GivenWhenThen with BeforeAndAfter{
  var readerUnderTest: OCRReader = _
  before {
    readerUnderTest = new OCRReader
  }
  
  def testSchemeForOCRReader(expectedOutput: String, fileName: String, thenString: String): Unit = {
    Given(s"file name with $expectedOutput}")
    When("reader reads file")
    val output = readerUnderTest.readInput(fileName)
    Then(thenString)
    assert (output == expectedOutput)
  }

  describe("OCR Reader") {
    it("should read all zeros file") {
      testSchemeForOCRReader("000000000", "/test1.txt", "output should be nine zeros")
    }
    it("should read all ones file") {
      testSchemeForOCRReader("111111111", "/test2.txt", "output should be nine ones")
    }
    it("should read all twos file") {
      testSchemeForOCRReader("222222222", "/test3.txt", "output should be nine ones")
    }

  }
}
