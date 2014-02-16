package pl.allegro.funkata

import org.scalatest.{BeforeAndAfter, FunSpec, GivenWhenThen}

class OCRReaderTest extends FunSpec with GivenWhenThen with BeforeAndAfter{
  var readerUnderTest: OCRReader
  before {
    readerUnderTest = new OCRReader
  }
  
  def testSchemeForOCRReader(expectedOutput: String, fileName: String, thenString: String): Unit = {
    given(s"file name with $expectedOutput}")
    when("reader reads file")
    val output = readerUnderTest.readInput(fileName)
    then(thenString)
    assert (output == expectedOutput)
  }

  describe("OCR Reader") {
    it("should read all zeros file") {
      testSchemeForOCRReader("000000000", "test1.txt", "output should be nine zeros")
    }

  }
}
