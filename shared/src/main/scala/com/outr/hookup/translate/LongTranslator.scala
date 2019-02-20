package com.outr.hookup.translate

import com.outr.hookup.data.{DataReader, DataWriter}

object LongTranslator extends Translator[Long] {
  override def write(value: Long, writer: DataWriter): DataWriter = writer.long(value)

  override def read(reader: DataReader): Long = reader.long()
}
