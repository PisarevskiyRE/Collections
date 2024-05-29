package homegrown.collections

trait FoldableFactory[+Element] extends Foldable[Element]{

  protected def empty: FoldableFactory[Nothing]

  def add[Super >: Element](input: Super): FoldableFactory[Super]


  def map[Result](function: Element => Result): FoldableFactory[Result] = {
    fold[FoldableFactory[Result]](empty) { (acc, current) =>
      acc.add(function(current))
    }
  }



}
