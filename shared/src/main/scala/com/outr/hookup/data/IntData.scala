package com.outr.hookup.data

import java.nio.ByteBuffer

case class IntData(value: Int) extends DataBlock {
  override def write(bb: ByteBuffer): Unit = bb.putInt(value)

  override def length: Int = 4
}
