package spec

import com.outr.hookup._
import io.circe.Json
import org.scalatest.wordspec.AsyncWordSpec
import org.scalatest.{BeforeAndAfterEach, Matchers}
import reactify.{Channel, Var}
import scribe.format.Formatter

import scala.concurrent.{Future, Promise}

class HookupSpec extends AsyncWordSpec with Matchers with BeforeAndAfterEach {
  "Interface" should {
    "set up logging" in {
      scribe.Logger.root.clearHandlers().withHandler(Formatter.enhanced, minimumLevel = Some(scribe.Level.Info)).replace()
      succeed
    }
    "set up a Hookup instance and test the expected parameter values" in {
      trait ClientInterface1 extends Hookup {
        val interface1: TestInterface1 with HookupSupport = create[TestInterface1]
      }
      val client = Hookup.client[ClientInterface1]
      val promise = Promise[Json]
      client.io.output.attach(promise.success)
      client.interface1.createUser("John Doe", 21, None)
      promise.future.map { json =>
        val params = (json \\ "params").head
        params should be(Json.obj(
          "name" -> Json.fromString("John Doe"),
          "age" -> Json.fromInt(21),
          "city" -> Json.Null
        ))
      }
    }
    "set up a Hookup instance with interface and implementation" in {
      trait ClientInterface1 extends Hookup {
        val interface1: TestInterface1 with HookupSupport = create[TestInterface1]
      }
      trait ServerInterface1 extends Hookup {
        val interface1: TestInterface1 = create[TestInterface1](Test1)
      }
      val client = Hookup.client[ClientInterface1]
      val server = Hookup.server[ServerInterface1]
      val serverInstance = server("test1")

      Hookup.connect.direct(client, serverInstance)
      client.interface1.reverse("This is a test!").map { result =>
        result should be("!tset a si sihT")
      }
    }
    "properly test a client / server implementation manually" in {
      trait Client extends Hookup {
        val interface: CommunicationInterface with HookupSupport = create[CommunicationInterface, ClientCommunicationInterface]
      }
      trait Server extends Hookup {
        val interface: CommunicationInterface with HookupSupport = create[CommunicationInterface, ServerCommunicationInterface]
      }
      val client = Hookup.client[Client]
      val server = Hookup.server[Server]
      val serverInstance = server("instance1")

      Hookup.connect.direct(client, serverInstance)

      client.interface.reverse("Hello, World!").map { result =>
        result should be("!dlroW ,olleH")
      }
    }
    "properly test a client / server implementation using auto" in {
      trait Communication extends Hookup {
        val interface: CommunicationInterface with HookupSupport = auto[CommunicationInterface]
      }
      val client = Hookup.client[Communication]
      val server = Hookup.server[Communication]
      val serverInstance = server("instance1")

      Hookup.connect.direct(client, serverInstance)

      client.interface.reverse("Hello, World!").flatMap { result =>
        result should be("!dlroW ,olleH")

        client.interface.logIn("user", "pass").flatMap { result =>
          result should be(true)

          client.interface.split("This,should,have,five,entries", ',').map { result =>
            result should be(List("This", "should", "have", "five", "entries"))
          }
        }
      }
    }
    "test HookupException on missing end-point" in {
      trait ClientInterface1 extends Hookup {
        val interface1: TestInterface1 with HookupSupport = create[TestInterface1]
      }
      trait ServerInterface1 extends Hookup {
        val interface1: TestInterface1 = create[TestInterface1]
      }
      val client = Hookup.client[ClientInterface1]
      val server = Hookup.server[ServerInterface1]
      val serverInstance = server("test1")

      Hookup.connect.direct(client, serverInstance)
      recoverToExceptionIf[HookupException] {
        client.interface1.reverse("This is a test!")
      }.map { exc =>
        exc.getMessage should be("No callable found for: spec.TestInterface1.reverse (NoCallable)")
      }
    }
    "test HookupException on exception thrown from implementation" in {
      trait ClientInterface1 extends Hookup {
        val interface1: TestInterface1 with HookupSupport = create[TestInterface1]
      }
      trait ServerInterface1 extends Hookup {
        val interface1: TestInterface1 = create[TestInterface1](Test1Fail)
      }
      val client = Hookup.client[ClientInterface1]
      val server = Hookup.server[ServerInterface1]
      val serverInstance = server("test1")

      Hookup.connect.direct(client, serverInstance)
      recoverToExceptionIf[HookupException] {
        client.interface1.reverse("This is a test!")
      }.map { exc =>
        exc.getMessage should be("Reverse failed! (java.lang.RuntimeException)")
      }
    }
    "test channel support" in {
      trait Interface extends Hookup {
        val greeting: Channel[String] = channel[String]
      }
      val i1 = Hookup.client[Interface]
      val i2 = Hookup.client[Interface]
      Hookup.connect.direct(i1, i2)
      val promise = Promise[String]
      i2.greeting.attach { value =>
        promise.success(value)
      }
      i1.greeting := "Hello, World!"
      promise.future.map { result =>
        result should be("Hello, World!")
      }
    }
    "test prop support" in {
      trait Interface extends Hookup {
        val greeting: Var[String] = prop[String]("")
      }
      val i1 = Hookup.client[Interface]
      val i2 = Hookup.client[Interface]
      Hookup.connect.direct(i1, i2)
      val promise1 = Promise[String]
      val promise2 = Promise[String]
      i2.greeting.attach { value =>
        if (!promise1.isCompleted) promise1.success(value)
      }
      i1.greeting := "Hello, World!"
      promise1.future.flatMap { result =>
        result should be("Hello, World!")
        i1.greeting() should be("Hello, World!")
        i2.greeting() should be("Hello, World!")

        i1.greeting.attach { value =>
          promise2.success(value)
        }
        i2.greeting := "Goodbye, World!"
        promise2.future.map { result =>
          result should be("Goodbye, World!")
          i1.greeting() should be("Goodbye, World!")
          i2.greeting() should be("Goodbye, World!")
        }
      }
    }
    "properly test using HookupManager" in {
      trait Client extends Hookup {
        val interface1: TestInterface1 with HookupSupport = create[TestInterface1]
      }
      trait Server extends Hookup {
        val interface1: TestInterface1 = create[TestInterface1](Test1)
      }
      trait Communication extends Hookup {
        val interface: CommunicationInterface with HookupSupport = auto[CommunicationInterface]
      }

      val client = Hookup.client[Client]
      Hookup.server[Server]
      val commClient = Hookup.client[Communication]
      Hookup.server[Communication]

      val clientManager = HookupManager.clients
      val serverManager = HookupManager("instance1", registerAllServers = true)

      clientManager.entries.size should be(2)
      serverManager.entries.size should be(2)
      clientManager.entries.flatMap(_.callables.keys) should be(Set("spec.TestInterface1", "spec.CommunicationInterface"))
      serverManager.entries.flatMap(_.callables.keys) should be(Set("spec.TestInterface1", "spec.CommunicationInterface"))

      Hookup.connect.direct(clientManager, serverManager)

      client.interface1.reverse("Hello, World!").flatMap { result =>
        result should be("!dlroW ,olleH")

        commClient.interface.logIn("user", "pass").flatMap { result =>
          result should be(true)

          commClient.interface.instance.map { key =>
            key should be("instance1")
          }
        }
      }
    }
  }

  override protected def afterEach(): Unit = {
    HookupManager.clear()
    HookupServer.clear()
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

trait CommunicationInterface {
  @server def reverse(value: String): Future[String]

  @server def logIn(username: String, password: String): Future[Boolean]

  @server def split(value: String, char: Char): Future[List[String]]

  @server def time: Future[Long]

  @server def instance: Future[String]
}

trait ServerCommunicationInterface extends CommunicationInterface with HookupSupport {
  override def reverse(value: String): Future[String] = Future.successful(value.reverse)

  override def logIn(username: String, password: String): Future[Boolean] = Future.successful(true)

  override def split(value: String, char: Char): Future[List[String]] = Future.successful(value.split(char).toList)

  override def time: Future[Long] = Future.successful(System.currentTimeMillis())

  override def instance: Future[String] = Future.successful(hookup.key.toString)
}

trait ClientCommunicationInterface extends CommunicationInterface