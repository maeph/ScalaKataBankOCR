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
    it("should read all zeros file") {
      checkThat("output should be nine zeros", "000000000", "/test1.txt" )
    }
    it("should read all ones file") {
      checkThat("output should be nine ones", "111111111", "/test2.txt")
    }
    it("should read all twos file") {
      checkThat("output should be nine twos", "222222222", "/test3.txt")
    }
    it("should read all threes file") {
      checkThat("output should be nine threes", "333333333", "/test4.txt" )
    }
    it("should read all fours file") {
      checkThat("output should be nine fours", "444444444", "/test5.txt" )
    }
    it("should read all fives file") {
      checkThat("output should be nine fives", "555555555", "/test6.txt")
    }
    it("should read all sixes file") {
      checkThat("output should be nine sixes", "666666666", "/test7.txt")
    }
    it("should read all sevens file") {
      checkThat("output should be nine sixes", "777777777", "/test8.txt")
    }
    it("should read all eights file") {
      checkThat("output should be nine sixes", "888888888", "/test9.txt")
    }
    it("should read all nines file") {
      checkThat("output should be nine sixes", "999999999", "/test10.txt")
    }
    it("should read different digits file") {
      checkThat("output should be 123456789", "123456789", "/test11.txt")
    }
    it("should validate correctness of digits") {
      checkThat("output should contain illegal message", "49006771? ILL", "/test_ill.txt")
    }
    



  }
}
