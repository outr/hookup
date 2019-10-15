package spec

import hookup.Hookup
import org.scalatest.wordspec.AsyncWordSpec
import org.scalatest.{BeforeAndAfterEach, Matchers}

import scala.concurrent.Future

class HookupSpec extends AsyncWordSpec with Matchers with BeforeAndAfterEach {
  "Hookup" should {
    "defined an interface" in {
      val h = Hookup[TestInterface1]
      h.local.isEmpty should be(true)
      h.remote.keySet should be(Set("reverse", "createUser"))
    }
  }
}

trait TestInterface1 {
  def reverse(value: String): Future[String]

  def createUser(name: String, age: Int, city: Option[String]): Future[User]
}

object Test1 extends TestInterface1 {
  override def reverse(value: String): Future[String] = Future.successful(value.reverse)

  override def createUser(name: String, age: Int, city: Option[String]): Future[User] = Future.successful {
    User(name, age, city)
  }
}

object Test1Fail extends TestInterface1 {
  override def reverse(value: String): Future[String] = throw new RuntimeException("Reverse failed!")

  override def createUser(name: String, age: Int, city: Option[String]): Future[User] = throw new RuntimeException("Create User failed!")
}

case class User(name: String, age: Int, city: Option[String])