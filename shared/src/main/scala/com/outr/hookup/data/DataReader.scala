package com.outr.hookup.data

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
    override def string(): String = new String(array())
    override def array(): Array[Byte] = {
      val length = int()
      val bytes = new Array[Byte](length)
      bb.get(bytes)
      bytes
    }
  }
}