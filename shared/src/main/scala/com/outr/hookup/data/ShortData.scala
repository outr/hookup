package com.outr.hookup.data

import java.nio.ByteBuffer


case class ShortData(value: Short) extends DataBlock {
  override def write(bb: ByteBuffer): Unit = bb.putShort(value)

  override def length: Int = 2
}











