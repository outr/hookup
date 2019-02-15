package com.outr.hookup.translate

import com.outr.hookup.data.{DataReader, DataWriter}

object IntTranslator extends Translator[Int] {
  override def read(reader: DataReader): Int = reader.int()

  override def write(value: Int, writer: DataWriter): DataWriter = writer.int(value)
}