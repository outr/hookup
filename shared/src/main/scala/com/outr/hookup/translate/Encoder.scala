package com.outr.hookup.translate

import java.nio.ByteBuffer

trait Encoder[Value] {
  def write(value: Value, bb: ByteBuffer): Unit
  def length(value: Value): Int
}