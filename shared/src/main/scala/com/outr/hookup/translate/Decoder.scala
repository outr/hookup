package com.outr.hookup.translate

import java.nio.ByteBuffer

trait Decoder[Value] {
  def read(bb: ByteBuffer): Value
}