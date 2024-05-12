package homegrown.collections

sealed trait Set[Element] extends (Element => Boolean) {
  import Set._

  final def apply2(input: Element): Boolean = {
    var result = false
    foreach { current =>
      result = result || current == input
    }
    result
  }

  final override def apply(input: Element): Boolean = {
    val seed = false

    var acc = seed
    foreach { current =>
      acc = acc || current == input
    }

    val result = acc
    result

    //    fold(seed = false) { (acc, current) =>
    //        acc || current == input
    //    }

  }
  final def fold[Result](seed: Result)(function: (Result, Element) => Result): Result = {
    if (isEmpty)
      seed
    else {
      //val NonEmpty(element, otherElements) = this

      val nonEmptySet = this.asInstanceOf[NonEmpty[Element]]
      val element = nonEmptySet.element
      val otherElements = nonEmptySet.otherElements

      val elementResult: Result = function(seed, element)
      val result: Result = otherElements.fold(elementResult)(function)

      result
    }
  }

  final def foreach[Result](function: Element => Result): Unit = {
    if (isEmpty)
      ()
    else {
      //val NonEmpty(element, otherElements) = this

      val nonEmptySet = this.asInstanceOf[NonEmpty[Element]]
      val element = nonEmptySet.element
      val otherElements = nonEmptySet.otherElements

      val elementResult: Unit = function(element)
      val result: Unit = otherElements.foreach(function)

      result
    }
  }

  final def add(input: Element): Set[Element] = {
    val seed = NonEmpty(input, empty)

    var acc = seed

    foreach { current =>
      if (input != current)
        acc = NonEmpty(current, acc)
    }

    val result = acc
    result
  }

  final def remove(input: Element): Set[Element] = {
    var result = empty[Element]
    foreach { current =>
      if (input != current)
        result = NonEmpty(current, result)
    }
    result
  }

  final def union(that: Set[Element]): Set[Element] = {
    var result = that
    foreach { current =>
      result = result.add(current)
    }
    result
  }

  final def intersection(that: Set[Element]): Set[Element] = {
    var result = empty[Element]
    foreach { current =>
      if (that(current))
        result = result.add(current)
    }
    result
  }

  final def difference(that: Set[Element]): Set[Element] = {
    var result = empty[Element]
    foreach { current =>
      if (!that(current))
        result = result.add(current)
    }
    result
  }
  final def isSubsetOf(that: Set[Element]): Boolean = {
    var result = true
    foreach { current =>
      result = result && that(current)
    }
    result
  }

  final def isSupersetOf(that: Set[Element]): Boolean =
    that.isSubsetOf(this)

  final override def equals(other: Any): Boolean = other match {
    case that: Set[Element] => this.isSubsetOf(that) && that.isSubsetOf(this)
    case _                  => false
  }

  final def size: Int = {
    var result = 0
    foreach { _ =>
      result = result + 1
    }
    result
  }

  final def isEmpty: Boolean =
    this.isInstanceOf[Empty[Element]]

  final def nonEmpty: Boolean =
    !isEmpty

  final def isSingleton: Boolean = {
    if (isEmpty)
      false
    else {
      val nonEmptySet = this.asInstanceOf[NonEmpty[Element]]
      val otherElements = nonEmptySet.otherElements

      otherElements.isEmpty
    }
  }

  final override def hashCode: Int = {
    var result = 41

    foreach { current =>
      result = result + current.hashCode
    }

    result
  }

  final def sample: Option[Element] = {
    if (isEmpty) {
      None
    }
    else {
      val nonEmptySet = this.asInstanceOf[NonEmpty[Element]]
      val element = nonEmptySet.element
      Some(element)
    }
  }

  final def map[Result](function: Element => Result): Set[Result] = {
    var result = empty[Result]
    foreach { current =>
      result = result.add(function(current))
    }
    result
  }

  //  final def map[Result](function: Element => Result): Set[Result] = {
  //    flatMap { current =>
  //      Set(function(current))
  //    }
  //  }

  final def flatMap[Result](function: Element => Set[Result]): Set[Result] = {
    var result = empty[Result]
    foreach { outerCurrent =>
      function(outerCurrent).foreach { innerCurrent =>
        result = result.add(innerCurrent)
      }
    }
    result

  }
}

object Set {
  def apply[Element](element: Element, otherElements: Element*): Set[Element] = {
    var result: Set[Element] = empty[Element].add(element)

    otherElements.foreach { current =>
      result = result.add(current)
    }

    result
  }

  private final case class NonEmpty[Element](element: Element, otherElements: Set[Element]) extends Set[Element]

  private object NonEmpty {
    private[this] def unapply(any: Any): Option[(String, Any)] =
      patternMatchingNotSupported
  }

  private class Empty[Element] extends Set[Element] {
    private[this] def unapply(any: Any): Option[(String, Any)] =
      patternMatchingNotSupported
  }

  private[this] def unapply(any: Any): Option[(String, Any)] =
    patternMatchingNotSupported

  private[this] def patternMatchingNotSupported: Nothing =
    sys.error("pattern matching on Sets is expensive and therefore not supported")

  def empty[Element]: Set[Element] = new Empty[Element]
}
