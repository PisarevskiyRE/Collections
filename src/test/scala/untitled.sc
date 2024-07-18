import java.time.LocalDate

var sum: Long = 1

for (i <- "Hello"){
  sum *= i
}

println(sum)


"Hello".map(x => x.toLong).product



def product(s: String): Long = {
  var sum: Long = 1

  for (i <- "Hello"){
    sum *= i
  }
  sum
}


def product(s: String): Long = {
  def loop(s: String, agg: Long ): Long = {
    if (s.isEmpty) {agg}
    else {
      loop(s.tail, agg * s.head)
    }
  }
  loop(s, 1)
}

product("Hello")


//
//implicit class DateInterpolator(val sc: StringContext) extends AnyVal {
//  def date(args: Any*): LocalDate = {
//
//  }
//}

(10 until 0 by -1).foreach(println)
