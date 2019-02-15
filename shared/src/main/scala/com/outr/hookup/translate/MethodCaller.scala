package com.outr.hookup.translate

import com.outr.hookup.HookupSupport
import com.outr.hookup.data.{DataReader, DataWriter}

import scala.concurrent.Future
import scribe.Execution.global

trait MethodCaller[Params, Result] {
  def paramsDecoder: Decoder[Params]
  def resultEncoder: Encoder[Result]

  def execute(support: HookupSupport,
              requestId: Long,
              reader: DataReader,
              writer: DataWriter): Future[DataWriter] = {
    val params = decode(reader)
    invoke(params).map { result =>
      val id = support.nextId()
      encode(result, writer.long(id))
    }
  }

  def invoke(params: Params): Future[Result]

  def decode(reader: DataReader): Params = paramsDecoder.read(reader)

  def encode(result: Result, writer: DataWriter): DataWriter = resultEncoder.write(result, writer)
}