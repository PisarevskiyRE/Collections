package homegrown.collections

trait Set extends (String => Boolean) {

  def add(input: String): Set
  def remove(input: String): Set

  //  final def remove(input: String): Set = element =>
  //    input != element && this(element)
  //
  //  final def union(that: Set): Set = element =>
  //    this(element) || that(element)
  //
  //  final def intersection(that: Set): Set = element =>
  //    this(element) && that(element)
  //
  //  final def difference(that: Set): Set = element =>
  //    this(element) && !that(element)
  //
  //  final def isSubsetOf(that: Set): Set = element =>
  //    this(element) && !that(element)

}

object Set {
  private final case class NonEmpty(element: String, otherElement: Set) extends Set {
    final override def apply(input: String): Boolean =
      input == element || otherElement(input)

    final override def add(input: String): Set = NonEmpty(input, otherElement.add(element))

    final override def remove(input: String): Set =
      if (input == element)
        otherElement
      else
        NonEmpty(element, otherElement.remove(input))
  }

  private object Empty extends Set {
    final override def apply(input: String): Boolean = false

    final override def add(input: String): Set = NonEmpty(input, Empty)

    final override def remove(input: String): Set = this
  }
  val empty: Set = Empty
}
