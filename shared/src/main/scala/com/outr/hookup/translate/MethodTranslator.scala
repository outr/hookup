package com.outr.hookup.translate

import java.nio.ByteBuffer

trait MethodTranslator[Params, Result] {
  def paramsEncoder: Encoder[Params]
  def resultDecoder: Decoder[Result]

  def encode(params: Params)(prepare: Int => ByteBuffer = length => ByteBuffer.allocate(length)): ByteBuffer = {
    val length = paramsEncoder.length(params)
    val bb = prepare(length)
    paramsEncoder.write(params, bb)
    bb
  }

  def decode(bb: ByteBuffer): Result = resultDecoder.read(bb)
}