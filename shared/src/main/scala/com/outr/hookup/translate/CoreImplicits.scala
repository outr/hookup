package com.outr.hookup.translate
import java.nio.ByteBuffer

trait CoreImplicits {
  private val SomeByte = 1.toByte
  private val NoneByte = 0.toByte

  implicit val intTranslator: Translator[Int] = IntTranslator
  implicit val stringTranslator: Translator[String] = StringTranslator
  implicit def optionTranslator[T](implicit tt: Translator[T]): Translator[Option[T]] = new Translator[Option[T]] {
    override def write(value: Option[T], bb: ByteBuffer): Unit = value match {
      case Some(t) => {
        bb.put(SomeByte)
        tt.write(t, bb)
      }
      case None => bb.put(NoneByte)
    }

    override def length(value: Option[T]): Int = value match {
      case Some(t) => 1 + tt.length(t)
      case None => 1
    }

    override def read(bb: ByteBuffer): Option[T] = bb.get() match {
      case SomeByte => Some(tt.read(bb))
      case NoneByte => None
    }
  }
}