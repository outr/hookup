package spec

import hookup.{Hookup, HookupQueue}
import io.circe.Json
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{BeforeAndAfterEach, Matchers}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class HookupSpec extends AnyWordSpec with Matchers with BeforeAndAfterEach {
  "Hookup" should {
    "define an interface" in {
      val queue = new HookupQueue
      val h = Test.interface(queue)
      h.local.isEmpty should be(true)
      queue.hasNext should be(false)
      val future = h.instance.reverse("Hello, World!")
      queue.hasNext should be(true)
      val request = queue.next().getOrElse(fail())
      request.id should be(1L)
      request.json should be(Json.obj(
        "endpoint" -> Json.fromString("reverse"),
        "params" -> Json.obj(
          "value" -> Json.fromString("Hello, World!")
        )
      ))
      future.isCompleted should be(false)
    }
    "define an implementation" in {
      val queue = new HookupQueue
      val h = Test.implementation(queue, Test1)
      h.local.isEmpty should be(false)
      val request = Json.obj(
        "endpoint" -> Json.fromString("reverse"),
        "params" -> Json.obj(
          "value" -> Json.fromString("Hello, World!")
        )
      )
      val future = h.receive(request)
      val result = Await.result(future, 1.second)
      result should be(Json.fromString("!dlroW ,olleH"))
    }
    "complete cycle" in {
      val queue = new HookupQueue
      val future = Test.fullTest(queue)
      val result = Await.result(future, 1.second)
      result should be("!dlroW ,olleH")
      queue.hasNext should be(false)
    }
    "complete cycle with error" in {
      val queue = new HookupQueue
      val future = Test.failTest(queue)
      val result = Await.result(future.failed, 1.second)
      result.getMessage should be("Reverse failed!")
      queue.hasNext should be(false)
    }
  }
}

object Test {
  import scribe.Execution.global

  def interface(queue: HookupQueue): Hookup[TestInterface1] = Hookup[TestInterface1](queue)
  def implementation(queue: HookupQueue, implementation: TestInterface1): Hookup[TestInterface1] = {
    Hookup[TestInterface1](queue, implementation)
  }

  def fullTest(queue: HookupQueue): Future[String] = {
    val interface = Test.interface(queue)
    val implementation = Test.implementation(queue, Test1)
    val future = interface.instance.reverse("Hello, World!")
    val request = queue.next().getOrElse(throw new RuntimeException("Request not in queue"))
    implementation.receive(request.json).map { json =>
      request.success(json)
    }
    future
  }

  def failTest(queue: HookupQueue): Future[String] = {
    val interface = Test.interface(queue)
    val implementation = Test.implementation(queue, Test1Fail)
    val future = interface.instance.reverse("Hello, World!")
    val request = queue.next().getOrElse(throw new RuntimeException("Request not in queue"))
    implementation.receive(request.json).failed.map { t =>
      request.failure(t)
    }
    future
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