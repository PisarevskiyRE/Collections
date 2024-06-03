package homegrown.collections

trait Factory[SubtypeOfFoldableFactory[+E] <: FoldableFactory[E, SubtypeOfFoldableFactory]] {

  def empty: SubtypeOfFoldableFactory[Nothing]

  def apply[Element](element: Element, otherElements: Element*): SubtypeOfFoldableFactory[Element] =
    otherElements.foldLeft[SubtypeOfFoldableFactory[Element]](empty.add(element))(_ add _)

}
