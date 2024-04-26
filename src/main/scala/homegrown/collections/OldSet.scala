package homegrown.collections

trait OldSet extends (String => Boolean) {

  final def add(input: String): OldSet = element =>
    input == element || this(element)

  final def remove(input: String): OldSet = element =>
    input != element && this(element)

  final def union(that: OldSet): OldSet = element =>
    this(element) || that(element)

  final def intersection(that: OldSet): OldSet = element =>
    this(element) && that(element)

  final def difference(that: OldSet): OldSet = element =>
    this(element) && !that(element)

  final def isSubsetOf(that: OldSet): OldSet = element =>
    this(element) && !that(element)
}

object OldSet {
  val empty: OldSet = input => false

  //  val empty: Set = new Set {
  //    override final def apply(input: String): Boolean = false
  //  }
}
