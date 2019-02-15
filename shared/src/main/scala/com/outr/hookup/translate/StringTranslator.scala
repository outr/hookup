package com.outr.hookup.translate

import com.outr.hookup.data.{DataReader, DataWriter}

object StringTranslator extends Translator[String] {
  override def read(reader: DataReader): String = reader.string()

  override def write(value: String, writer: DataWriter): DataWriter = writer.string(value)
}