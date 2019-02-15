package spec

import com.outr.hookup.{Hookup, server}
import com.outr.hookup.translate.Translator
import org.scalatest.{AsyncWordSpec, Matchers}

import scala.concurrent.Future

class HookupSpec extends AsyncWordSpec with Matchers {
  implicit val userTranslator: Translator[User] = Hookup.translator[User]

  "Interface" should {
    "properly translate using a MethodTranslator" in {
      val method = Hookup.method[TestInterface1, String, String]("reverse")
      val bb = method.encode("Hello, World!")()
      bb.flip()
      method.decode(bb) should be("Hello, World!")
    }
    "properly call user a MethodCaller" in {
      val method = Hookup.method[TestInterface1, String, String]("reverse", Test1)
      method.invoke("Hello, World!").map { result =>
        result should be("!dlroW ,olleH")
      }
    }
    "properly follow the entire cycle" in {
      val remote = Hookup.method[TestInterface1, String, String]("reverse")
      val bb1 = remote.encode("Hello, World!")()
      bb1.flip()

      val local = Hookup.method[TestInterface1, String, String]("reverse", Test1)
      val params = local.decode(bb1)
      local.invoke(params).map { result =>
        val bb2 = local.encode(result)()
        bb2.flip()
        val finalResult = remote.decode(bb2)
        finalResult should be("!dlroW ,olleH")
      }
    }
    "properly test a custom implementation" in {
      val local = Hookup.create[TestInterface1]
      val remote = Hookup.create[TestInterface1](Test1)

      local.interfaceName should be("spec.HookupSpec.TestInterface1")
      remote.interfaceName should be("spec.HookupSpec.TestInterface1")
      local.hashCode() should be(remote.hashCode())

      Hookup.connect.direct(local, remote)

      local.reverse("Hello, World!").map { result =>
        result should be("!dlroW ,olleH")
      }
    }
    "properly text a client / server implementation" in {
      val client = Hookup.client[CommunicationInterface, ClientInterface]
      val server = Hookup.server[CommunicationInterface, ServerInterface]

      Hookup.connect.direct(client, server)

      client.reverse("Hello, World!").map { result =>
        result should be("!dlroW ,olleH")
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

  case class User(name: String, age: Int, city: Option[String])

  trait CommunicationInterface {
    @server def reverse(value: String): Future[String]
  }

  trait ServerInterface extends CommunicationInterface {
    override def reverse(value: String): Future[String] = Future.successful(value.reverse)
  }

  trait ClientInterface extends CommunicationInterface
}