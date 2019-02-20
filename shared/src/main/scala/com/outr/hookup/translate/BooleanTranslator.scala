package com.outr.hookup.translate

import com.outr.hookup.data.{DataReader, DataWriter}

object BooleanTranslator extends Translator[Boolean] {
  override def write(value: Boolean, writer: DataWriter): DataWriter = writer.boolean(value)

  override def read(reader: DataReader): Boolean = reader.boolean()
}
