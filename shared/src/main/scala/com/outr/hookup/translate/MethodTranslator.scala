package com.outr.hookup.translate

import com.outr.hookup.data.{DataReader, DataWriter}

trait MethodTranslator[Params, Result] {
  def paramsEncoder: Encoder[Params]
  def resultDecoder: Decoder[Result]

  def encode(params: Params, writer: DataWriter): DataWriter = {
    paramsEncoder.write(params, writer)
  }

  def decode(reader: DataReader): Result = resultDecoder.read(reader)
}