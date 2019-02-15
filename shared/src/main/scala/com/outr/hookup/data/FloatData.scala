package com.outr.hookup.data

import java.nio.ByteBuffer

case class FloatData(value: Float) extends DataBlock {
  override def write(bb: ByteBuffer): Unit = bb.putFloat(value)

  override def length: Int = 4
}
