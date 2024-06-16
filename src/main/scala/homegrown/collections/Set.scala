package homegrown.collections

sealed abstract class Set[+Element] extends FoldableFactory[Element, Set] {
  import Set._

  final override protected def factory: Factory[Set] =
    Set

  //@scala.annotation.tailrec
  final override def fold[Result](seed: Result)(function: (Result, Element) => Result): Result =
    this match {
      case _: Empty.type =>
        seed
      case nonEmptySet: NonEmpty[Element] =>
        val currentResult = function(seed, nonEmptySet.element)
        val rightResult = nonEmptySet.right.fold(currentResult)(function)
        nonEmptySet.left.fold(rightResult)(function)
    }

  final def apply[Super >: Element](input: Super): Boolean =
    contains(input)

  final override def add[Super >: Element](input: Super): Set[Super] =
    this match {
      case _: Empty.type =>
        NonEmpty(empty, input, empty)
      case nonEmptySet: NonEmpty[Element] =>
        if (input == nonEmptySet.element)
          nonEmptySet
        else if (input.hashCode() <= nonEmptySet.element.hashCode())
          NonEmpty(nonEmptySet.left.add(input), nonEmptySet.element, nonEmptySet.right)
        else
          NonEmpty(nonEmptySet.left, nonEmptySet.element, nonEmptySet.right.add(input))
    }

  final override def remove[Super >: Element](input: Super): Set[Super] =
  this match {
    case _: Empty.type =>
     empty
    case nonEmptySet: NonEmpty[Element] =>
      if (input == nonEmptySet.element)
        nonEmptySet
      else if (input.hashCode() <= nonEmptySet.element.hashCode())
        NonEmpty(nonEmptySet.left.add(input), nonEmptySet.element, nonEmptySet.right)
      else
        NonEmpty(nonEmptySet.left, nonEmptySet.element, nonEmptySet.right.add(input))
  }

  final def union[Super >: Element](that: Set[Super]): Set[Super] =
    fold(that)(_ add _)

  final def intersection(predicate: Element => Boolean): Set[Element] =
    filter(predicate)

  final def difference(predicate: Element => Boolean): Set[Element] =
    fold[Set[Element]](empty) { (acc, current) =>
      if (predicate(current))
        acc
      else
        acc.add(current)
    }

  final def isSubsetOf(predicate: Element => Boolean): Boolean =
    fold(true) { (acc, current) =>
      acc && predicate(current)
    }

  final def isSupersetOf[Super >: Element](that: Set[Super]): Boolean =
    that.isSubsetOf(this)

  final override def equals(other: Any): Boolean = other match {
    case that: Set[Element] => this.isSubsetOf(that) && that.isSubsetOf(this)
    case _                  => false
  }

  final override def hashCode: Int =
    fold(41) { (acc, current) =>
      acc + current.hashCode()
    }

  final def isEmpty: Boolean =
    this.isInstanceOf[Empty.type]

  final def nonEmpty: Boolean =
    !isEmpty

  def isSingleton: Boolean

  def sample: Option[Element]
}

object Set extends Factory[Set] {

  private final case class NonEmpty[Element](left: Set[Element], element: Element, right: Set[Element]) extends Set[Element] {

    final override def toString(): String = {
      "{" + element + splitByCommaSpace(left) + splitByCommaSpace(right) + "}"
    }

    final override def isSingleton: Boolean =
      left.isEmpty && right.isEmpty

    final override def sample: Option[Element] =
      Some(element)

    private[this] def splitByCommaSpace(input: Set[Element]) = input.fold("") { (acc, current) =>
      s"$acc, $current"
    }
  }

  private object NonEmpty {
    private[this] def unapply(any: Any): Option[(String, Any)] =
      patternMatchingNotSupported
  }

  private object Empty extends Set[Nothing] {

    final override def toString(): String =
      "{}"

    final override def isSingleton: Boolean =
      false

    final override def sample: Option[Nothing] =
      None

    private[this] def unapply(any: Any): Option[(String, Any)] =
      patternMatchingNotSupported
  }

  private[this] def unapply(any: Any): Option[(String, Any)] =
    patternMatchingNotSupported

  private[this] def patternMatchingNotSupported: Nothing =
    sys.error("pattern matching on Sets is expensive and therefore not supported")

  final override def empty: Set[Nothing] = Empty

  implicit def SetCanBeUsedAsFunction1[Element](set: Set[Element]): Element => Boolean =
    set.apply
}

sealed trait CompanyRole {
  def id: String
  final def roleName: String = getClass.toString
}

final case class Employee(id: String) extends CompanyRole {
  final def takeVacation(): Unit = {
    println("taking a vacation")
  }
}

final case class Consultant(id: String, companyName: String) extends CompanyRole {
  final def submitInvoice(): Unit = {
    println("here is my invoice")
  }
}
