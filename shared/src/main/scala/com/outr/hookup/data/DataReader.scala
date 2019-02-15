package com.outr.hookup.data

import java.nio.ByteBuffer

trait DataReader {
  def boolean(): Boolean
  def byte(): Byte
  def char(): Char
  def short(): Short
  def int(): Int
  def long(): Long
  def float(): Float
  def double(): Double
  def string(): String
  def array(): Array[Byte]
}

object DataReader {
  def apply(bb: ByteBuffer): DataReader = new DataReader {
    override def boolean(): Boolean = bb.get() == 1.toByte
    override def byte(): Byte = bb.get()
    override def char(): Char = bb.getChar
    override def short(): Short = bb.getShort
    override def int(): Int = bb.getInt
    override def long(): Long = bb.getLong
    override def float(): Float = bb.getFloat
    override def double(): Double = bb.getDouble
    override def string(): String = new String(array(), "UTF-8")
    override def array(): Array[Byte] = {
      val length = int()
      val bytes = new Array[Byte](length)
      bb.get(bytes)
      bytes
    }
  }

  def apply(blocks: List[DataBlock]): DataReader = new DataReader {
    private var queued = blocks

    private def get[B <: DataBlock]: B = synchronized {
      val head = queued.head.asInstanceOf[B]
      queued = queued.tail
      head
    }

    override def boolean(): Boolean = get[BooleanData].value
    override def byte(): Byte = get[ByteData].value
    override def char(): Char = get[CharData].value
    override def short(): Short = get[ShortData].value
    override def int(): Int = get[IntData].value
    override def long(): Long = get[LongData].value
    override def float(): Float = get[FloatData].value
    override def double(): Double = get[DoubleData].value
    override def string(): String = get[StringData].value
    override def array(): Array[Byte] = get[ArrayData].array
  }
}