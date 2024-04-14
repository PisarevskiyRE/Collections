
object Main extends App{
  println("-"*50)

  code(args)

  println("-"*50)

  private def code(args: Array[String]): Unit = {
    args
      .headOption             // Option[String]
      .map(CreditCard.apply)  // Option[CreditCard]
      .map(println)           // Option[Unit]
      .getOrElse(runDemo)
  }

  private def runDemo(): Unit = {
    val validCard: CreditCard.Valid =
      CreditCard()

    println(validCard)
    println(validCard.number)
    println(validCard.isValid)


    println()

    val invalidCard: CreditCard =
      CreditCard("1234567812345678")

    println(invalidCard)
    println(invalidCard.number)
    println(invalidCard.isValid)
  }
}
