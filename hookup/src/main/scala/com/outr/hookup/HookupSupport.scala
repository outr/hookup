package com.outr.hookup

import java.util.concurrent.atomic.AtomicLong

import io.circe.Json

import scala.concurrent.{Future, Promise}
import scribe.Execution.global

import scala.util.{Failure, Random, Success}

trait HookupSupport extends HookupIO {
  private val id = Random.nextLong()
  private var _disposed: Boolean = false
  private val idGenerator = new AtomicLong(0L)
  private var callbacks = Map.empty[Long, Promise[Json]]

  init()

  def interfaceName: String
  def disposed: Boolean = _disposed

  /**
    * Locally defined methods that can be invoked remotely
    */
  def callables: Map[String, HookupCallable]

  private def init(): Unit = {
    io.input.attach { json =>
      val id = (json \\ "id").head.asNumber.get.toLong.get
      (json \\ "type").head.asString.get match {
        case HookupSupport.`type`.Invoke => {
          val name = (json \\ "name").head.asString.get
          val params = (json \\ "params").head
          callables.get(name) match {
            case Some(callable) => {
              try {
                callable.call(params).onComplete {
                  case Success(value) => io.output := HookupSupport.response(id, name, value)
                  case Failure(throwable) => {
                    scribe.debug(s"Error while invoking $interfaceName.$name")
                    io.output := HookupSupport.error(id, name, Option(throwable.getMessage).getOrElse("Error"), throwable.getClass.getName)
                  }
                }
              } catch {
                case t: Throwable => {
                  scribe.debug(s"Error while invoking $interfaceName.$name: ${t.getMessage}")
                  io.output := HookupSupport.error(id, name, Option(t.getMessage).getOrElse("Error"), t.getClass.getName)
                }
              }
            }
            case None => {
              val message = s"No callable found for: $name"
              scribe.debug(message)
              io.output := HookupSupport.error(id, name, message, "NoCallable")
            }
          }
        }
        case HookupSupport.`type`.Response => {
          val response = (json \\ "response").head
          callbacks.get(id) match {
            case Some(callback) => {
              scribe.debug(s"Found callback with $id ($interfaceName)")
              HookupSupport.this.synchronized {
                callbacks -= id
              }
              callback.success(response)
            }
            case None => scribe.debug(s"$interfaceName ($this): No callback found for $json (${callbacks.keySet})")
          }
        }
        case HookupSupport.`type`.Error => {
          val message = (json \\ "message").head.asString.get
          val errorType: String = (json \\ "errorType").head.asString.get
          callbacks.get(id) match {
            case Some(callback) => {
              HookupSupport.this.synchronized {
                callbacks -= id
              }
              callback.failure(HookupException(message, errorType))
            }
            case None => throw new RuntimeException(s"No callback found for $json")
          }
        }
      }
    }
  }

  protected def remoteInvoke(name: String, params: Json): Future[Json] = synchronized {
    assert(!disposed, "Hookup is disposed")
    val id = idGenerator.incrementAndGet()
    val promise = Promise[Json]
    callbacks += id -> promise
    scribe.debug(s"RemoteInvoke: $name / $id: $this (${callbacks.keySet})")
    io.output := HookupSupport.invoke(id, name, params)
    promise.future
  }

  def dispose(): Unit = synchronized {
    _disposed = true
    callbacks.values.foreach { promise =>
      promise.failure(HookupException("Disposed", "Disposal"))
    }
    callbacks = Map.empty
  }

  override def toString: String = s"${getClass.getName}.$id"
}

object HookupSupport {
  object `type` {
    val Invoke = "invoke"
    val Response = "response"
    val Error = "error"
    val Channel = "channel"
  }

  def invoke(id: Long, name: String, params: Json): Json = Json.obj(
    "id" -> Json.fromLong(id),
    "type" -> Json.fromString(HookupSupport.`type`.Invoke),
    "name" -> Json.fromString(name),
    "params" -> params
  )

  def response(id: Long, name: String, value: Json): Json = Json.obj(
    "id" -> Json.fromLong(id),
    "name" -> Json.fromString(name),
    "type" -> Json.fromString(HookupSupport.`type`.Response),
    "response" -> value
  )

  def error(id: Long, name: String, message: String, errorType: String): Json = Json.obj(
    "id" -> Json.fromLong(id),
    "name" -> Json.fromString(name),
    "type" -> Json.fromString(HookupSupport.`type`.Error),
    "message" -> Json.fromString(message),
    "errorType" -> Json.fromString(errorType)
  )

  def channel(id: Long, name: String, value: Json): Json = Json.obj(
    "id" -> Json.fromLong(id),
    "name" -> Json.fromString(name),
    "type" -> Json.fromString(HookupSupport.`type`.Channel),
    "value" -> value
  )
}