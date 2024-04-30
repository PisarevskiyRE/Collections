package homegrown.collections

sealed trait Set extends (String => Boolean) {
  import Set._

  final override def apply(input: String): Boolean = {
    var result = false

    foreach { current =>
      result = result || current == input
    }
    result
  }

  final def add(input: String): Set = {
    var result = NonEmpty(input, empty)

    foreach { current =>
      if (input != current)
        result = NonEmpty(current, result)
    }
    result
  }
  final def remove(input: String): Set = {
    var result = empty

    foreach { current =>
      if (input != current)
        result = NonEmpty(current, result)
    }

    result
  }

  def union(that: Set): Set
  def intersection(that: Set): Set
  def difference(that: Set): Set

  def isSubsetOf(that: Set): Boolean

  final def isSupersetOf(that: Set): Boolean =
    that.isSubsetOf(this)

  final override def equals(other: Any): Boolean = other match {
    case that: Set => this.isSubsetOf(that) && that.isSubsetOf(this)
    case _         => false
  }

  def size: Int

  final def isEmpty: Boolean =
    this eq Set.empty

  final def nonEmpty: Boolean =
    !isEmpty

  def isSingleton: Boolean

  def sample: Option[String]

  def foreach(function: String => Unit): Unit
}

object Set {

  def apply(element: String, otherElements: String*): Set = {
    var result: Set = empty.add(element)

    otherElements.foreach { current =>
      result = result.add(current)
    }
    result
  }

  private final case class NonEmpty(element: String, otherElements: Set) extends Set {
    final override def union(that: Set): Set =
      otherElements.union(that.add(element))

    final override def intersection(that: Set): Set = {
      val intersectionOfOthers = otherElements.intersection(that)

      if (that(element))
        intersectionOfOthers.add(element)
      else
        intersectionOfOthers
    }

    final override def difference(that: Set): Set = {
      val differenceOfOthers = otherElements.difference(that)

      if (that(element))
        differenceOfOthers
      else
        differenceOfOthers.add(element)
    }

    final override def isSubsetOf(that: Set): Boolean =
      that(element) && otherElements.isSubsetOf(that)

    final override def hashCode: Int =
      element.hashCode + otherElements.hashCode

    final override def size: Int =
      1 + otherElements.size

    final override def isSingleton: Boolean =
      otherElements.isEmpty

    final override def sample: Option[String] =
      Some(element)

    final override def foreach(function: String => Unit): Unit = {
      function(element)
      otherElements.foreach(function)
    }
  }

  private object Empty extends Set {
    final override def union(that: Set): Set =
      that

    final override def intersection(that: Set): Set =
      this

    final override def difference(that: Set): Set =
      this

    final override def isSubsetOf(that: Set): Boolean =
      true

    final override def size: Int =
      0

    final override def isSingleton: Boolean =
      false

    final override def sample: Option[String] =
      None

    final override def foreach(function: String => Unit): Unit = ()
  }

  val empty: Set = Empty
}
