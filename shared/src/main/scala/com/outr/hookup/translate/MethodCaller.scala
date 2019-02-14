package com.outr.hookup.translate

import java.nio.ByteBuffer

import com.outr.hookup.InterfaceSupport

import scala.concurrent.Future
import scribe.Execution.global

trait MethodCaller[Params, Result] {
  def paramsDecoder: Decoder[Params]
  def resultEncoder: Encoder[Result]

  def execute(support: InterfaceSupport, requestId: Long, bb: ByteBuffer): Future[ByteBuffer] = {
    val params = decode(bb)
    invoke(params).map { result =>
      val id = support.nextId()
      encode(result)(support.prepareResponseFunction(id, requestId))
    }
  }

  def invoke(params: Params): Future[Result]

  def decode(bb: ByteBuffer): Params = paramsDecoder.read(bb)

  def encode(result: Result)(prepare: Int => ByteBuffer = length => ByteBuffer.allocate(length)): ByteBuffer = {
    val length = resultEncoder.length(result)
    val bb = prepare(length)
    resultEncoder.write(result, bb)
    bb
  }
}