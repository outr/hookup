package com.outr.hookup.data

import java.nio.ByteBuffer

trait DataBlock {
  def write(bb: ByteBuffer): Unit
  def length: Int
}
