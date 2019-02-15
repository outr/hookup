package com.outr.hookup.translate

import com.outr.hookup.data.DataWriter

trait Encoder[Value] {
  def write(value: Value, writer: DataWriter): DataWriter
}