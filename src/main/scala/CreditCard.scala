
sealed trait CreditCard {
  import CreditCard._

  def number: String

  final def isValid: Boolean =
    isInstanceOf[Valid]

  final def isNotValid: Boolean =
    !isValid

  final override def toString: String =
    if (isNotValid) {
      val invalid = Console.RED + "Invalid" + Console.RESET

      s"""$invalid credit card number "$number""""
    } else {
      val valid = Console.GREEN + "Valid" + Console.RESET
      val (payload, checkDigit) = split(number)

      s"""$valid credit card number "$number" with "$payload" and check digit "$checkDigit""""
    }
}

object CreditCard extends (String => CreditCard){

  object Invalid{
    private[CreditCard] def apply(number: String): Invalid =
      new Invalid(number)
  }
  final case class Invalid private (number: String) extends CreditCard


  object Valid{
    private[CreditCard] def apply(number: String): Valid =
      new Valid(number)
  }
  final case class   Valid private (number: String) extends CreditCard

  def apply(number: String): CreditCard =
    if (isValid(number))
      Valid(number)
    else
      Invalid(number)

  private val ChekDigitLeng = 1
  private val MinimumLength = 13
  private val MaximumLength = 19



  private def isValid(number: String): Boolean = {
    number != null &&
      number.nonEmpty &&
      number.forall(Character.isDigit) &&
      (MinimumLength to MaximumLength).contains(number.length) &&
      doesMAthCheckOut(number)
  }

  private def doesMAthCheckOut(number: String): Boolean = {
    val (payload, checkDigit) = split(number)
    val sum = luhn(payload) + checkDigit

    sum % 10 == 0
  }

  private def luhn(payload: String): Int =
    payload
      .reverse                  // String
      .map(_.toString.toInt)// IndexedSeq[Int]
      .zipWithIndex //IndexedSeq[(Int, Int)]
      .map {
        case (digit, index) =>
          if (index %2 ==0) digit *2
          else digit
      }.map { number =>
         if (number > 9 )
           number - 9
         else
           number
      }.sum

  private def split(number: String): (String, Int) = {
    val payload = number.dropRight(ChekDigitLeng)
    val checkDigit = number.takeRight(ChekDigitLeng).toInt

    payload -> checkDigit
  }


  def apply(): Valid =
    Valid(generateNumber)

  private def generateNumber(): String = {
    "123123"
  }
}
