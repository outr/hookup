package com.outr.hookup.data

import java.nio.ByteBuffer

case class BooleanData(value: Boolean) extends DataBlock {
  override def write(bb: ByteBuffer): Unit = bb.put(if (value) 1.toByte else 0.toByte)

  override def length: Int = 1
}
