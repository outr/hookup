package hookup

import io.circe._

import scala.concurrent.{Future, Promise}
import scala.language.experimental.macros

trait Hookup[Interface] {
  def queue: HookupQueue
  def local: Map[String, Json => Future[Json]]
  def instance: Interface
  def implementation: Option[Interface]

  /**
    * Supply JSON to invoke a local method on Interface and return Future[Json]
    *
    * @param json the invocation JSON code with parameters
    * @return Future[Json] of the return
    */
  def receive(json: Json): Future[Json] = try {
    val endPoint = (json \\ "endpoint").head.asString.getOrElse(throw new RuntimeException(s"No 'method' entry defined for: $json"))
    val lastDot = endPoint.lastIndexOf('.')
    val methodName = if (lastDot != -1) {
      endPoint.substring(lastDot + 1)
    } else {
      endPoint
    }
    val method = local.getOrElse(methodName, throw new RuntimeException(s"No local method found for name: $methodName (${local.keySet.mkString(", ")})"))
    method(json)
  } catch {
    case t: Throwable => Future.failed(t)
  }
}

object Hookup {
  def apply[Interface](queue: HookupQueue): Hookup[Interface] = macro HookupMacros.interface[Interface]
  def apply[Interface](queue: HookupQueue, implementation: Interface): Hookup[Interface] = macro HookupMacros.implementation[Interface]
}