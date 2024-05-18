package homegrown.collections

import org.graalvm.compiler.asm.amd64.AMD64VectorAssembler.VexFloatCompareOp.Predicate

sealed trait Set[Element] extends (Element => Boolean) {
  import Set._

  final override def apply(input: Element): Boolean =
    contains(input)

  final def contains(input: Element): Boolean =
    exists(_ == input)

  final def doesNotContain(input: Element): Boolean =
    !contains(input)

  final def doesNotExist(predicate: Element => Boolean): Boolean =
    !exists(predicate)

  final def exists(predicate: Element => Boolean): Boolean =
    fold(false)(_ || predicate(_))

  final def notForall(predicate: Element => Boolean): Boolean =
    !forall(predicate)

  final def forall(predicate: Element => Boolean): Boolean =
    fold(true)(_ && predicate(_))



  final def foreach[Result](function: Element => Result): Unit = {
    fold(()) { (_, current) =>
      function(current)
    }
  }

  final def add(input: Element): Set[Element] =
    fold(NonEmpty(input, empty)) { (acc, current) =>
      if (current == input)
        acc
      else
        NonEmpty(current, acc)
    }

  final def remove(input: Element): Set[Element] =
    fold(empty[Element]) { (acc, current) =>
      if (current == input)
        acc
      else
        NonEmpty(current, acc)
    }

  final def union(that: Set[Element]): Set[Element] =
    fold(that)(_ add _)

  final def intersection(predicate: Element => Boolean): Set[Element] =
    filter(predicate)

  final def filter(predicate: Element => Boolean): Set[Element] =
    fold(empty[Element]) { (acc, current) =>
      if (predicate(current))
        acc.add(current)
      else
        acc
    }

  final def difference(predicate: Element => Boolean): Set[Element] =
    fold(empty[Element]) { (acc, current) =>
      if (predicate(current))
        acc
      else
        acc.add(current)
    }

  final def isSubsetOf(predicate: Element => Boolean): Boolean =
    fold(true) { (acc, current) =>
      acc && predicate(current)
    }

  final def isSupersetOf(that: Set[Element]): Boolean =
    that.isSubsetOf(this)

  final override def equals(other: Any): Boolean = other match {
    case that: Set[Element] => this.isSubsetOf(that) && that.isSubsetOf(this)
    case _                  => false
  }

  final override def hashCode: Int =
    fold(41) { (acc, current) =>
      acc + current.hashCode()
    }

  final override def toString(): String =
    if (isEmpty)
      "Set()"
    else
      ???

  final def size: Int = {
    fold(0) { (acc, current) =>
      acc + 1
    }
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
    fold(empty[Result]) { (acc, current) =>
      acc.add(function(current))
    }
  }

  final def flatMap[Result](function: Element => Set[Result]): Set[Result] =
    fold(empty[Result]) { (outerAcc, outerCurrent) =>
      function(outerCurrent).fold(outerAcc) { (innerAcc, innerCurrent) =>
        innerAcc.add(innerCurrent)
      }
    }



  @scala.annotation.tailrec
  final def fold[Result](seed: Result)(function: (Result, Element) => Result): Result =
    if (isEmpty)
      seed
    else {
      otherElementsOrThrowException.fold(function(seed, elementOrThrowException))(function)
    }



  private[this] lazy val (elementOrThrowException, otherElementsOrThrowException) = {
    val nonEmptySet = this.asInstanceOf[NonEmpty[Element]]
    val element = nonEmptySet.element
    val otherElements = nonEmptySet.otherElements

    element -> otherElements
  }
}

object Set {
  def apply[Element](element: Element, otherElements: Element*): Set[Element] =
    otherElements.foldLeft(empty[Element].add(element)) { (acc, current) =>
      acc.add(current)
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
