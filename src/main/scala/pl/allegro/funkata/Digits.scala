package pl.allegro.funkata

/**
 * Created by mephi_000 on 16.02.14.
 */
object Digits {

  val ONE:String =
    """%   
       %  |
       %  |
       %   """.stripMargin('%')
  val TWO:String =
    """% _ 
       % _|
       %|_ 
       %   """.stripMargin('%')
  val THREE:String =
    """% _ 
       % _|
       % _|
       %   """.stripMargin('%')
  val FOUR:String =
    """%   
       %|_|
       %  |
       %   """.stripMargin('%')
  val FIVE:String =
    """% _ 
       %|_ 
       % _|
       %   """.stripMargin('%')
  val SIX:String =
    """% _ 
       %|_ 
       %|_|
       %   """.stripMargin('%')
  val SEVEN:String =
    """% _ 
       %  |
       %  |
       %   """.stripMargin('%')
  val EIGHT:String =
    """% _ 
       %|_|
       %|_|
       %   """.stripMargin('%')
  val NINE:String =
    """% _ 
       %|_|
       % _|
       %   """.stripMargin('%')
  val ZERO:String =
    """% _ 
       %| |
       %|_|
       %   """.stripMargin('%')

  def digitalToDigit: (String) => String = {

    case Digits.ZERO => "0"
    case Digits.ONE => "1"
    case Digits.TWO => "2"
    case Digits.THREE => "3"
    case Digits.FOUR => "4"
    case Digits.FIVE => "5"
    case Digits.SIX => "6"
    case Digits.SEVEN => "7"
    case Digits.EIGHT => "8"
    case Digits.NINE => "9"
    case _ => "?"

  }
}
/**
    _  _     _  _  _  _  _  _
  | _| _||_||_ |_   ||_||_|| |
  ||_  _|  | _||_|  ||_| _||_|
  **/