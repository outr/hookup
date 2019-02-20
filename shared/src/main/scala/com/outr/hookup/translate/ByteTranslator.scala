package com.outr.hookup.translate

import com.outr.hookup.data.{DataReader, DataWriter}

object ByteTranslator extends Translator[Byte] {
  override def write(value: Byte, writer: DataWriter): DataWriter = writer.byte(value)

  override def read(reader: DataReader): Byte = reader.byte()
}
