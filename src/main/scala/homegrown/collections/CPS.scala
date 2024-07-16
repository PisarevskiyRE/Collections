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
    val five = cps(5) { five =>
      val fifteen = five + identity(10)

      println(fifteen)
    }
  }

  println("─" * 25)

  {
    cps(5) { five =>
      val fifteen = five + cps(10)(identity[Int])

      println(fifteen)
    }
  }

  println("─" * 25)

  {
    cps(5) { five =>
      cps(five + 10) { fifteen =>
        println(fifteen)
      }
    }
  }

  println("─" * 25)

  {
    cps[Int, Unit](5) { five =>
      cps[Int, Unit](five + 10) { fifteen =>
        cps(fifteen)(println)
      }
    }
  }

  println("─" * 25)

  import scala.util.continuations._

  {
    reset{
      val five = shift(cps[Int, Unit](5))
      val fifteen = five + shift(cps[Int, Unit](10))
      println(fifteen)
    }

  }

  println("─" * 25)

  {
    reset{
      def shiftCps(input: Int): Int @cpsParam[Unit, Unit] = shift(cps[Int, Unit](input))


      val five = shiftCps(5)
      val fifteen = five + shiftCps(10)
      println(fifteen)
    }

  }

  println("─" * 50)
}
