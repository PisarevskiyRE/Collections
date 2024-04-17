import org.scalatest.funsuite._
import org.scalatest.matchers.should.Matchers



class SetSuite extends AnyFunSuite with Matchers {

  test("test success"){

    Set.empty.size shouldBe 0


    "hello world" should include ("hell")

    "hello world" should not include ("!")


    Set.empty.isEmpty shouldBe true
  }

}

