package hookup

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicLong

import io.circe.Json

import scala.concurrent.{Future, Promise}

class HookupQueue {
  private val idGenerator = new AtomicLong(0L)
  private val queue = new ConcurrentLinkedQueue[HookupRequest]

  def enqueue(json: Json): Future[Json] = {
    val promise = Promise[Json]
    queue.add(HookupRequest(idGenerator.incrementAndGet(), json, promise))
    promise.future
  }
  def next(): Option[HookupRequest] = Option(queue.poll())
  def hasNext: Boolean = !queue.isEmpty
}
