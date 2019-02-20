package com.outr.hookup.translate

import com.outr.hookup.data.{DataReader, DataWriter}

object ArrayTranslator extends Translator[Array[Byte]] {
  override def write(value: Array[Byte], writer: DataWriter): DataWriter = writer.array(value)

  override def read(reader: DataReader): Array[Byte] = reader.array()
}
