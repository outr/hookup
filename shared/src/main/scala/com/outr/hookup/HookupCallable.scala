package com.outr.hookup

import io.circe.Json

import scala.concurrent.Future

/**
  * HookupCallable is a mapping to a locally implemented method that can be invoked remotely
  */
trait HookupCallable {
  def name: String
  def call(json: Json): Future[Json]
}