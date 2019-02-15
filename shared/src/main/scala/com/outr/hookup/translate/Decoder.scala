package com.outr.hookup.translate

import com.outr.hookup.data.DataReader

trait Decoder[Value] {
  def read(reader: DataReader): Value
}