package com.outr.hookup.translate

import com.outr.hookup.data.{DataReader, DataWriter}

object ShortTranslator extends Translator[Short] {
  override def write(value: Short, writer: DataWriter): DataWriter = writer.short(value)

  override def read(reader: DataReader): Short = reader.short()
}
