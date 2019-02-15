package com.outr.hookup

import java.nio.ByteBuffer
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

import com.outr.hookup.data.{DataReader, DataWriter}
import com.outr.hookup.translate.MethodCaller
import scribe.Execution.global

trait HookupSupport extends HookupIO {
  def interfaceName: String

  private val idGenerator: AtomicLong = new AtomicLong(0L)
  protected val methodFutures = new ConcurrentHashMap[Long, DataReader => Unit]()
  protected val methodMap: Map[String, MethodCaller[Any, Any]]

  input.attach { reader =>
    // Determine action
    reader.byte() match {
      case Hookup.Action.MethodRequest => {
        val requestId = reader.long()
        val methodName = reader.string()
        val methodHandler = methodMap.getOrElse(methodName, throw new RuntimeException(s"No method found by name in method request: $methodName"))
        methodHandler.execute(this, requestId, reader, DataWriter.empty).map { writer =>
          output := writer
        }.failed.foreach { t =>
          // TODO: send back MethodResponseError
          scribe.error(s"MethodRequest failure for $requestId, method: $methodName", t)
        }
      }
      case Hookup.Action.MethodResponse => {
        val id = reader.long()
        val requestId = reader.long()
        try {
          val method = Option(methodFutures.get(requestId))
            .getOrElse(throw new RuntimeException(s"No request id for received MethodResponse: $requestId (id: $id)"))
          method(reader)
        } catch {
          case t: Throwable => {
            // TODO: Better error handling
            scribe.error(s"MethodResponse failure for $requestId (id: $id)", t)
          }
        }
      }
    }
  }

  def nextId(): Long = idGenerator.getAndIncrement()

  protected def prepareRequestFunction(id: Long, methodName: String): Int => ByteBuffer = messageLength => {
    val methodNameBytes = methodName.getBytes
    // Action + ID + Method Name + Message Length
    val length = 1 + 8 + 4 + methodNameBytes.length + messageLength
    val bb = ByteBuffer.allocate(length)
    bb.put(Hookup.Action.MethodRequest)
    bb.putLong(id)
    bb.putInt(methodNameBytes.length)
    bb.put(methodNameBytes)
    bb
  }

  def prepareResponseFunction(id: Long, requestId: Long): Int => ByteBuffer = messageLength => {
    // Action + ID + Request ID + Message Length
    val length = 1 + 8 + 8 + messageLength
    val bb = ByteBuffer.allocate(length)
    bb.put(Hookup.Action.MethodResponse)
    bb.putLong(id)
    bb.putLong(requestId)
    bb
  }
}