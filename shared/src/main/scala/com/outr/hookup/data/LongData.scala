package com.outr.hookup.data

import java.nio.ByteBuffer

case class LongData(value: Long) extends DataBlock {
  override def write(bb: ByteBuffer): Unit = bb.putLong(value)

  override def length: Int = 8
}
