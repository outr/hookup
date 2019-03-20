package com.outr.hookup

import java.util.concurrent.atomic.AtomicLong

import io.circe.Json

import scala.concurrent.{Future, Promise}
import scribe.Execution.global

import scala.util.{Failure, Success}

trait HookupSupport extends HookupIO {
  private val idGenerator = new AtomicLong(0L)
  private var callbacks = Map.empty[Long, Promise[Json]]

  io.input.attach { json =>
    (json \\ "type").head.asString.get match {
      case HookupSupport.`type`.Invoke => {
        val id = (json \\ "id").head.asNumber.get.toLong.get
        val name = (json \\ "name").head.asString.get
        val params = (json \\ "params").head
        callables.get(name) match {
          case Some(callable) => {
            callable.call(params).onComplete {
              case Success(value) => io.output := Json.obj(
                "id" -> Json.fromLong(id),
                "type" -> Json.fromString(HookupSupport.`type`.Response),
                "response" -> value
              )
              case Failure(throwable) => {
                scribe.error(s"Error while invoking $interfaceName.$name", throwable)
                io.output := Json.obj(
                  "id" -> Json.fromLong(id),
                  "type" -> Json.fromString(HookupSupport.`type`.Error),
                  "message" -> Json.fromString(Option(throwable.getMessage).getOrElse("Error")),
                  "class" -> Json.fromString(throwable.getClass.getName)
                )
              }
            }
          }
          case None => scribe.error(s"No callable found for: $name")
        }
      }
      case HookupSupport.`type`.Response => {
        val id = (json \\ "id").head.asNumber.get.toLong.get
        val response = (json \\ "response").head
        callbacks.get(id) match {
          case Some(callback) => {
            HookupSupport.this.synchronized {
              callbacks -= id
            }
            callback.success(response)
          }
          case None => throw new RuntimeException(s"No callback found for $json")
        }
      }
      case HookupSupport.`type`.Error => {
        val id = (json \\ "id").head.asNumber.get.toLong.get
        val message = (json \\ "message").head.asString.get
        val `class` = (json \\ "class").head.asString.get
        callbacks.get(id) match {
          case Some(callback) => {
            HookupSupport.this.synchronized {
              callbacks -= id
            }
            callback.failure(throw HookupException(message, `class`))
          }
          case None => throw new RuntimeException(s"No callback found for $json")
        }
      }
    }
  }

  def interfaceName: String

  def callables: Map[String, HookupCallable]

  protected def remoteInvoke(name: String, params: Json): Future[Json] = synchronized {
    val id = idGenerator.incrementAndGet()
    val promise = Promise[Json]
    callbacks += id -> promise
    io.output := Json.obj(
      "id" -> Json.fromLong(id),
      "type" -> Json.fromString(HookupSupport.`type`.Invoke),
      "name" -> Json.fromString(name),
      "params" -> params
    )
    promise.future
  }
}

object HookupSupport {
  object `type` {
    val Invoke = "invoke"
    val Response = "response"
    val Error = "error"
  }
}

/**
  * HookupCallable is a mapping to a locally implemented method that can be invoked remotely
  */
trait HookupCallable {
  def name: String
  def call(json: Json): Future[Json]
}

case class HookupException(message: String, remoteClass: String) extends RuntimeException(message)