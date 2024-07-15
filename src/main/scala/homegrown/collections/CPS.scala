package homegrown.collections

object CPS extends App {
  println("─" * 50)

  {
    val five = 5
    val fifteen = five + 10

    println(fifteen)
  }
  println("─" * 25)



  {
    val five = identity(5)
    val fifteen = five + identity(10)

    println(fifteen)
  }

  println("─" * 25)

  def cps[Input, ContinuationResult](input: Input)(continuation: Input => ContinuationResult): ContinuationResult =
    continuation(input)

  {
    val five = cps(5)(identity[Int])
    val fifteen = five + identity(10)

    println(fifteen)
  }

  println("─" * 25)

  {
    val five = cps(5){ five =>
      val fifteen = five + identity(10)

      println(fifteen)
    }
  }


  println("─" * 25)

  {
    val five = cps(5){ five =>
      val fifteen = five + cps(10)(identity[Int])

      println(fifteen)
    }
  }

  println("─" * 50)
}
