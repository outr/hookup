package com.outr.hookup.data

import java.nio.ByteBuffer

case class ArrayData(array: Array[Byte]) extends DataBlock {
  override def write(bb: ByteBuffer): Unit = {
    bb.putInt(array.length)
    bb.put(array)
  }

  override def length: Int = 4 + array.length
}
