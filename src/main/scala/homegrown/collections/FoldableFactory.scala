package homegrown.collections

trait FoldableFactory[+Element, SubtypeOfFoldableFactory[+Element] <: FoldableFactory[Element, SubtypeOfFoldableFactory]] extends Foldable[Element] {

  protected def factory: Factory[Nothing]

  def add[Super >: Element](input: Super): SubtypeOfFoldableFactory[Super]
  def remove[Super >: Element](input: Super): SubtypeOfFoldableFactory[Super]

  def map[Result](function: Element => Result): SubtypeOfFoldableFactory[Result] = {
    fold[SubtypeOfFoldableFactory[Result]](factory.empty)(_ add function(_))
  }

  def flatMap[Result](function: Element => Foldable[Result]): SubtypeOfFoldableFactory[Result] =
    fold[SubtypeOfFoldableFactory[Result]](factory.empty) { (outerAcc, outerCurrent) =>
      function(outerCurrent).fold(outerAcc) { (innerAcc, innerCurrent) =>
        innerAcc.add(innerCurrent)
      }
    }

  final def filterNot(predicate: Element => Boolean): SubtypeOfFoldableFactory[Element] =
    filter(input => !predicate(input))

  def filter(predicate: Element => Boolean): SubtypeOfFoldableFactory[Element] =
    fold[SubtypeOfFoldableFactory[Element]](factory.empty) { (acc, current) =>
      if (predicate(current))
        acc.add(current)
      else
        acc
    }

}
