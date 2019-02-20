package com.outr.hookup.translate

import com.outr.hookup.data.{DataReader, DataWriter}

object FloatTranslator extends Translator[Float] {
  override def write(value: Float, writer: DataWriter): DataWriter = writer.float(value)

  override def read(reader: DataReader): Float = reader.float()
}
