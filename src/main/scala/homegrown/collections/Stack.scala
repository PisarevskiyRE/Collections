package homegrown.collections

import scala.annotation.tailrec

object Main extends App {

  println("-" * 50)

  val max = 15000
  println(max)

  {
    var acc = 0
    while (acc < max) {
      acc += 1
    }
    println(acc)
  }

  {
    @tailrec
    def loop(acc: Int): Int = {
      if (acc < max)
        loop(acc + 1)
      else
        acc
    }

    println(loop(acc = 0))
  }

  {
    //@tailrec
    def loop(acc: Int): Int = {
      if (acc < max)
        loop(acc + 1) + 0
      else
        acc
    }

    println(loop(acc = 0))
  }

  println("-" * 50)

}
