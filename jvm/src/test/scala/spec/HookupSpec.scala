package spec

import com.outr.hookup.data.{DataReader, DataWriter}
import com.outr.hookup.{Hookup, HookupManager, HookupSupport, server}
import com.outr.hookup.translate.Translator
import org.scalatest.{AsyncWordSpec, Matchers}

import scala.concurrent.Future

class HookupSpec extends AsyncWordSpec with Matchers {
  implicit val userTranslator: Translator[User] = Hookup.translator[User]

  "Interface" should {
    "properly translate using a MethodTranslator" in {
      val method = Hookup.method[TestInterface1, String, String]("reverse")
      val writer = method.encode("Hello, World!", DataWriter.empty)
      val reader = DataReader(writer.blocks)
      method.decode(reader) should be("Hello, World!")
    }
    "properly call user a MethodCaller" in {
      val method = Hookup.method[TestInterface1, String, String]("reverse", Test1)
      method.invoke("Hello, World!").map { result =>
        result should be("!dlroW ,olleH")
      }
    }
    "properly follow the entire cycle" in {
      val remote = Hookup.method[TestInterface1, String, String]("reverse")
      val writer = remote.encode("Hello, World!", DataWriter.empty)

      val local = Hookup.method[TestInterface1, String, String]("reverse", Test1)
      val params = local.decode(DataReader(writer.blocks))
      local.invoke(params).map { result =>
        val writer2 = local.encode(result, DataWriter.empty)
        val finalResult = remote.decode(DataReader(writer2.blocks))
        finalResult should be("!dlroW ,olleH")
      }
    }
    "properly test a custom implementation" in {
      val local = Hookup.create[TestInterface1]
      val remote = Hookup.create[TestInterface1](Test1)

      local.interfaceName should be("spec.TestInterface1")
      remote.interfaceName should be("spec.TestInterface1")
      local.hashCode() should be(remote.hashCode())

      Hookup.connect.direct(local, remote)

      local.reverse("Hello, World!").map { result =>
        result should be("!dlroW ,olleH")
      }
    }
    "properly test a custom implementation using ByteBuffer encoding/decoding" in {
      val local = Hookup.create[TestInterface1]
      val remote = Hookup.create[TestInterface1](Test1)

      local.interfaceName should be("spec.TestInterface1")
      remote.interfaceName should be("spec.TestInterface1")
      local.hashCode() should be(remote.hashCode())

      Hookup.connect.bytes(local, remote)

      local.reverse("Hello, World!").map { result =>
        result should be("!dlroW ,olleH")
      }
    }
    "properly text a client / server implementation" in {
      val client = Hookup.client[CommunicationInterface, ClientCommunicationInterface]
      val server = Hookup.server[CommunicationInterface, ServerCommunicationInterface]

      Hookup.connect.direct(client, server)

      client.reverse("Hello, World!").map { result =>
        result should be("!dlroW ,olleH")
      }
    }
    "properly create HookupManagers and communicate between them" in {
      val local = HookupManager.client[TestManager]
      val remote = HookupManager.server[TestManager].create()

      Hookup.connect.direct(local, remote)

      local.communication.reverse("Hello, World!").map { result =>
        result should be("!dlroW ,olleH")
      }
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

trait ServerCommunicationInterface extends CommunicationInterface {
  override def reverse(value: String): Future[String] = Future.successful(value.reverse)
}

trait ClientCommunicationInterface extends CommunicationInterface

trait TestManager extends HookupManager {
  val communication: CommunicationInterface with HookupSupport = {
    register(Hookup.auto[CommunicationInterface])
  }
}