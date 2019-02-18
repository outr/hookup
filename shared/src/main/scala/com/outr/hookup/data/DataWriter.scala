package com.outr.hookup.data

import java.nio.ByteBuffer

case class DataWriter(blocks: List[DataBlock] = Nil) extends DataBlock {
  def withBlock(block: DataBlock, prepend: Boolean = false): DataWriter = if (prepend) {
    block match {
      case DataWriter(other) => DataWriter(other ::: blocks)
      case _ => DataWriter(block :: blocks)
    }
  } else {
    block match {
      case DataWriter(other) => DataWriter(blocks ::: other)
      case _ => DataWriter(blocks ::: List(block))
    }
  }

  def boolean(value: Boolean, prepend: Boolean = false): DataWriter = withBlock(BooleanData(value), prepend)
  def byte(value: Byte, prepend: Boolean = false): DataWriter = withBlock(ByteData(value), prepend)
  def char(value: Char, prepend: Boolean = false): DataWriter = withBlock(CharData(value), prepend)
  def short(value: Short, prepend: Boolean = false): DataWriter = withBlock(ShortData(value), prepend)
  def int(value: Int, prepend: Boolean = false): DataWriter = withBlock(IntData(value), prepend)
  def long(value: Long, prepend: Boolean = false): DataWriter = withBlock(LongData(value), prepend)
  def float(value: Float, prepend: Boolean = false): DataWriter = withBlock(FloatData(value), prepend)
  def double(value: Double, prepend: Boolean = false): DataWriter = withBlock(DoubleData(value), prepend)
  def string(value: String, prepend: Boolean = false): DataWriter = withBlock(StringData(value), prepend)
  def array(value: Array[Byte], prepend: Boolean = false): DataWriter = withBlock(ArrayData(value), prepend)

  override def write(bb: ByteBuffer): Unit = blocks.foreach(_.write(bb))

  def length: Int = blocks.foldLeft(0)((sum, block) => sum + block.length)

  def toByteBuffer: ByteBuffer = {
    val bb = ByteBuffer.allocate(length)
    write(bb)
    bb.flip()
    bb
  }

  override def toString: String = s"DataWriter($blocks)"
}

object DataWriter {
  lazy val empty: DataWriter = DataWriter()
}