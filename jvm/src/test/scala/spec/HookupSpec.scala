package spec

import java.util.UUID

import com.outr.hookup.{Hookup, HookupManager}
import org.scalatest.{AsyncWordSpec, Matchers}

import scala.concurrent.Future

class HookupSpec extends AsyncWordSpec with Matchers {
  "Interface" should {
    "set up a HookupManager" in {
      val test1 = Hookup[TestInterface1]
      test1.create() should not be null
    }

    /*"properly translate using a MethodTranslator" in {
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

      local.communication.reverse("Hello, World!").flatMap { result =>
        result should be("!dlroW ,olleH")

        local.communication.logIn("user", "pass").flatMap { result =>
          result should be(true)

          local.communication.split("This,should,have,five,entries", ',').map { result =>
            result should be(List("This", "should", "have", "five", "entries"))
          }
        }
      }
    }
    "properly create HookupManagers and communicate between them with secondary communication" in {
      val local = HookupManager.client[TestManager]
      val remote = HookupManager.server[TestManager].create()

      Hookup.connect.direct(local, remote)

      local.comm2.uuid.map { result =>
        result should have size 36
      }
    }*/
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

/*
trait CommunicationInterface {
  @server def reverse(value: String): Future[String]

  @server def logIn(username: String, password: String): Future[Boolean]

  @server def split(value: String, char: Char): Future[List[String]]
}

trait ServerCommunicationInterface extends CommunicationInterface {
  override def reverse(value: String): Future[String] = Future.successful(value.reverse)

  override def logIn(username: String, password: String): Future[Boolean] = Future.successful(true)

  override def split(value: String, char: Char): Future[List[String]] = Future.successful(value.split(char).toList)
}

trait ClientCommunicationInterface extends CommunicationInterface

trait Comm2 {
  @server def uuid: Future[String]
}

trait ServerComm2 extends Comm2 {
  override def uuid: Future[String] = Future.successful(UUID.randomUUID().toString)
}

trait ClientComm2 extends Comm2

trait TestManager extends HookupManager {
  val communication: CommunicationInterface with HookupSupport = {
    register(Hookup.auto[CommunicationInterface])
  }
  val comm2: Comm2 with HookupSupport = register(Hookup.auto[Comm2])
}*/
