package com.outr.hookup.data

import java.nio.ByteBuffer

case class StringData(value: String) extends DataBlock {
  private lazy val array = value.getBytes

  override def write(bb: ByteBuffer): Unit = {
    bb.putInt(array.length)
    bb.put(array)
  }

  override def length: Int = 4 + array.length
}
