package spec

import com.outr.hookup.Interface
import com.outr.hookup.translate.Translator
import org.scalatest.{AsyncWordSpec, Matchers}

import scala.concurrent.Future

class InterfaceSpec extends AsyncWordSpec with Matchers {
  implicit val userTranslator: Translator[User] = Interface.translator[User]

  "Interface" should {
    "properly translate using a MethodTranslator" in {
      val method = Interface.method[TestInterface1, String, String]("reverse")
      val bb = method.encode("Hello, World!")()
      bb.flip()
      method.decode(bb) should be("Hello, World!")
    }
    "properly call user a MethodCaller" in {
      val method = Interface.method[TestInterface1, String, String]("reverse", Test1)
      method.invoke("Hello, World!").map { result =>
        result should be("!dlroW ,olleH")
      }
    }
    "properly follow the entire cycle" in {
      val remote = Interface.method[TestInterface1, String, String]("reverse")
      val bb1 = remote.encode("Hello, World!")()
      bb1.flip()

      val local = Interface.method[TestInterface1, String, String]("reverse", Test1)
      val params = local.decode(bb1)
      local.invoke(params).map { result =>
        val bb2 = local.encode(result)()
        bb2.flip()
        val finalResult = remote.decode(bb2)
        finalResult should be("!dlroW ,olleH")
      }
    }
    "properly test a custom implementation" in {
      val local = Interface.create[TestInterface1]
      val remote = Interface.create[TestInterface1](Test1)

      // TODO: simplify things with a ByteBufferBuilder

      local.output.attach { bb =>
        scribe.info("Local Output -> Remote Input")
        remote.input := bb
      }
      remote.output.attach { bb =>
        scribe.info("Remote Output -> Local Input")
        local.input := bb
      }
      local.input.on {
        scribe.info("Local Input")
      }
      remote.input.on {
        scribe.info("Remote Input")
      }

      local.reverse("Hello, World!").map { result =>
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