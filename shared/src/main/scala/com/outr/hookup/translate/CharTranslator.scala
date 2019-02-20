package com.outr.hookup.translate

import com.outr.hookup.data.{DataReader, DataWriter}

object CharTranslator extends Translator[Char] {
  override def write(value: Char, writer: DataWriter): DataWriter = writer.char(value)

  override def read(reader: DataReader): Char = reader.char()
}
