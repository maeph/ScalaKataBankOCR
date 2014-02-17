package pl.allegro.funkata

import org.scalatest.{BeforeAndAfter, FunSpec, GivenWhenThen}

class OCRReaderTest extends FunSpec with GivenWhenThen with BeforeAndAfter{
  var readerUnderTest: OCRReader = _
  before {
    readerUnderTest = new OCRReader
  }
  
  def testSchemeForOCRReader(expectedOutput: String, fileName: String, thenString: String): Unit = {
    Given(s"file name with $expectedOutput")
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
      testSchemeForOCRReader("222222222", "/test3.txt", "output should be nine twos")
    }
    it("should read all threes file") {
      testSchemeForOCRReader("333333333", "/test4.txt", "output should be nine threes")
    }
    it("should read all fours file") {
      testSchemeForOCRReader("444444444", "/test5.txt", "output should be nine fours")
    }
    it("should read all fives file") {
      testSchemeForOCRReader("555555555", "/test6.txt", "output should be nine fives")
    }
    it("should read all sixes file") {
      testSchemeForOCRReader("666666666", "/test7.txt", "output should be nine sixes")
    }
    it("should read all sevens file") {
      testSchemeForOCRReader("777777777", "/test8.txt", "output should be nine sixes")
    }
    it("should read all eights file") {
      testSchemeForOCRReader("888888888", "/test9.txt", "output should be nine sixes")
    }
    it("should read all nines file") {
      testSchemeForOCRReader("999999999", "/test10.txt", "output should be nine sixes")
    }
    it("should read different digits file") {
      testSchemeForOCRReader("123456789", "/test11.txt", "output should be 123456789")
    }
    



  }
}
