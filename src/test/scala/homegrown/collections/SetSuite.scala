import homegrown.collections._
import org.scalatest._

object SetSuite extends FunSuite with Matchers {

  test("apply on an empty Set should yield false") {
    Set.empty(randomString) shouldBe false
  }

  test("add on an empty Set should yield a new Set with one element") {

    val first = randomString
    val second = randomString

    first should not be second

    val set = Set.empty.add(first)

    set(first) shouldBe true
    set(second) shouldBe false

  }

  private def randomString: String =
    scala.util.Random.alphanumeric.take(5).mkString

  test("add on a non empty Set should yield a new Set with two element") {

    val first = randomString
    val second = randomString

    first should not be second

    val set = Set.empty.add(first).add(second)

    set(first) shouldBe true
    set(second) shouldBe true

  }


  test("remove on a non empty Set should yield a new Set without the element") {

    val element = randomString
    val setWithElement = Set.empty.add(element)

    setWithElement(element) shouldBe true

    val setWithoutElement = setWithElement.remove(element)

    setWithoutElement(element) shouldBe false
  }


  test("test1") {

    val first = randomString
    val second = randomString

    val setWithElement = Set.empty.add(first).add(second)

    setWithElement(first) shouldBe true
    setWithElement(second) shouldBe true

   val setWithoutElement = setWithElement.remove(first)
    setWithoutElement(first) shouldBe false
    setWithoutElement(second) shouldBe true

  }

  test("test2") {

    val first = randomString
    val second = randomString

    val setWithElement = Set.empty.add(first).add(second)

    setWithElement(first) shouldBe true
    setWithElement(second) shouldBe true

    val setWithoutElement = setWithElement.remove(second)
    setWithoutElement(first) shouldBe true
    setWithoutElement(second) shouldBe false

  }
}
