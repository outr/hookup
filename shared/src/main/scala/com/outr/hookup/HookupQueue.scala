package com.outr.hookup

import java.util.concurrent.ConcurrentLinkedQueue

import io.circe.Json

class HookupQueue {
  private val queue = new ConcurrentLinkedQueue[Json]

  def isEmpty: Boolean = queue.isEmpty
  def nonEmpty: Boolean = !isEmpty
  def +=(json: Json): Unit = queue.add(json)
  def poll(): Option[Json] = Option(queue.poll())
  def peek(): Option[Json] = Option(queue.peek())
}