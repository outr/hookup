package com.outr.hookup.translate

import java.nio.ByteBuffer

object IntTranslator extends Translator[Int] {
  override def write(value: Int, bb: ByteBuffer): Unit = bb.putInt(value)

  override def length(value: Int): Int = 4

  override def read(bb: ByteBuffer): Int = bb.getInt
}