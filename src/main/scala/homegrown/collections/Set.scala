package homegrown.collections

trait Set extends (String => Boolean) {

  final def add(input: String): Set = element =>
    input == element || this(element)

  final def remove(input: String): Set = element =>
    input != element || !this(element)
}

object Set {
  val empty: Set = input => false

  //  val empty: Set = new Set {
  //    override final def apply(input: String): Boolean = false
  //  }
}
