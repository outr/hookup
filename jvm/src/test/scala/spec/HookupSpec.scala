package spec

import java.util.UUID

import com.outr.hookup.{Hookup, HookupSupport, server}
import org.scalatest.{AsyncWordSpec, Matchers}

import scala.concurrent.Future

class HookupSpec extends AsyncWordSpec with Matchers {
  "Interface" should {
    "set up a Hookup instance" in {
      trait ClientInterface1 extends Hookup {
        val interface1: TestInterface1 with HookupSupport = create[TestInterface1]
      }
      trait ServerInterface1 extends Hookup {
        val interface1: TestInterface1 = create[TestInterface1](Test1)
      }
      val client = Hookup.client[ClientInterface1]
      val server = Hookup.server[ServerInterface1, String]
      val serverInstance = server("test1")

      Hookup.connect.direct(client, serverInstance)
      client.interface1.reverse("This is a test!").map { result =>
        result should be("!tset a si sihT")
      }
    }
    /*"properly test a client / server implementation manually" in {
//      trait Client extends Hookup {
//        val interface: CommunicationInterface with HookupSupport = create[CommunicationInterface, ClientCommunicationInterface]
//      }
      trait Server extends Hookup {
        val interface: CommunicationInterface with HookupSupport = create[CommunicationInterface, ServerCommunicationInterface]
      }
//      val client = Hookup.client[Client]
//      val server = Hookup.server[Server, String]
//      val serverInstance = server("instance1")

//      Hookup.connect.direct(client, serverInstance)

//      client.interface.reverse("Hello, World!").map { result =>
//        result should be("!dlroW ,olleH")
//      }
//      server should not be null
      succeed
    }*/
    /*"properly test a client / server implementation using auto" in {
      trait Communication extends Hookup {
        val interface: CommunicationInterface with HookupSupport = auto[CommunicationInterface]
      }
      val client = Hookup.client[Communication]
      val server = Hookup.server[Communication, String]
      val serverInstance = server("instance1")

      Hookup.connect.direct(client, serverInstance)

      client.interface.reverse("Hello, World!").map { result =>
        result should be("!dlroW ,olleH")
      }
    }*/
    /*"properly create HookupManagers and communicate between them" in {
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


trait CommunicationInterface {
  @server def reverse(value: String): Future[String]

//  @server def logIn(username: String, password: String): Future[Boolean]

//  @server def split(value: String, char: Char): Future[List[String]]
}

trait ServerCommunicationInterface extends CommunicationInterface {
  override def reverse(value: String): Future[String] = Future.successful(value.reverse)

//  override def logIn(username: String, password: String): Future[Boolean] = Future.successful(true)

//  override def split(value: String, char: Char): Future[List[String]] = Future.successful(value.split(char).toList)
}

trait ClientCommunicationInterface extends CommunicationInterface

trait Comm2 {
  @server def uuid: Future[String]
}

trait ServerComm2 extends Comm2 {
  override def uuid: Future[String] = Future.successful(UUID.randomUUID().toString)
}

trait ClientComm2 extends Comm2