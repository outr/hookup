package com.outr.hookup.translate

import com.outr.hookup.data.{DataReader, DataWriter}

object DoubleTranslator extends Translator[Double] {
  override def write(value: Double, writer: DataWriter): DataWriter = writer.double(value)

  override def read(reader: DataReader): Double = reader.double()
}
