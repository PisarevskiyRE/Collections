package homegrown.collections





sealed abstract class Stack[+Element]{
  import Stack._

  final def push[Super >: Element](input: Super): Stack[Super] =
    NonEmpty(input, this)

  final lazy val (peek, pop) = popElement

  final def popElement: (Option[Element], Stack[Element])=
    this match {
      case Empty =>
        None -> empty
      case NonEmpty(element, otherElements) =>
        Some(element) -> otherElements
    }

}

object Stack{
  final case class NonEmpty[+Element](element: Element, otherElements: Stack[Element]) extends Stack[Element]
  final case object Empty extends Stack[Nothing]

  def empty: Stack[Nothing] = Empty
}





//object Main extends App {
//
//  println("-" * 50)
//
//  val max = 15000
//  println(max)
//
//  {
//    var acc = 0
//    while (acc < max) {
//      acc += 1
//    }
//    println(acc)
//  }
//
//  {
//    @tailrec
//    def loop(acc: Int): Int = {
//      if (acc < max)
//        loop(acc + 1)
//      else
//        acc
//    }
//
//    println(loop(acc = 0))
//  }
//
//  {
//    //@tailrec
//    def loop(acc: Int): Int = {
//      if (acc < max)
//        loop(acc + 1) + 0
//      else
//        acc
//    }
//
//    println(loop(acc = 0))
//  }
//
//  println("-" * 50)
//
//}



