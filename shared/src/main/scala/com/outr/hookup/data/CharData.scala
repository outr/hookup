package com.outr.hookup.data

import java.nio.ByteBuffer

case class CharData(value: Char) extends DataBlock {
  override def write(bb: ByteBuffer): Unit = bb.putChar(value)

  override def length: Int = 2
}
