package com.outr.hookup.data

import java.nio.ByteBuffer

case class DoubleData(value: Double) extends DataBlock {
  override def write(bb: ByteBuffer): Unit = bb.putDouble(value)

  override def length: Int = 8
}
