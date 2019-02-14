package com.outr.hookup.translate

import java.nio.ByteBuffer

object StringTranslator extends Translator[String] {
  override def read(bb: ByteBuffer): String = {
    val length = bb.getInt
    val array = new Array[Byte](length)
    bb.get(array)
    new String(array)
  }

  override def write(value: String, bb: ByteBuffer): Unit = {
    val array = value.getBytes
    bb.putInt(array.length)
    bb.put(array)
  }

  override def length(value: String): Int = 4 + value.length
}