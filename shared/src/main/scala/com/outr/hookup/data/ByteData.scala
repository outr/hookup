package com.outr.hookup.data

import java.nio.ByteBuffer

case class ByteData(value: Byte) extends DataBlock {
  override def write(bb: ByteBuffer): Unit = bb.put(value)

  override def length: Int = 1
}
