package homegrown.collections

trait FoldableFactory[+Element, SubtypeOfFoldableFactory[+Element] <: FoldableFactory[Element, SubtypeOfFoldableFactory]] extends Foldable[Element] {

  protected def empty: SubtypeOfFoldableFactory[Nothing]

  def add[Super >: Element](input: Super): SubtypeOfFoldableFactory[Super]

  def map[Result](function: Element => Result): SubtypeOfFoldableFactory[Result] = {
    fold[SubtypeOfFoldableFactory[Result]](empty)(_ add function(_))
  }

}
