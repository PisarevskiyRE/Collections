package homegrown.collections

trait Foldable[+Element] {
  def fold[Result](seed: Result)(function: (Result, Element) => Result): Result

  final def doesNotContain[Super >: Element](input: Super): Boolean =
    !contains(input)

   def contains[Super >: Element](input: Super): Boolean =
    exists(_ == input)

  final def doesNotExist(predicate: Element => Boolean): Boolean =
    !exists(predicate)

  final def exists(predicate: Element => Boolean): Boolean =
    fold(false)(_ || predicate(_))

  final def notForall(predicate: Element => Boolean): Boolean =
    !forall(predicate)

  final def forall(predicate: Element => Boolean): Boolean =
    fold(true)(_ && predicate(_))


}
