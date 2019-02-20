package com.outr.hookup.translate

import com.outr.hookup.data.{DataReader, DataWriter}

trait CoreImplicits {
  implicit val arrayTranslator: Translator[Array[Byte]] = ArrayTranslator
  implicit val booleanTranslator: Translator[Boolean] = BooleanTranslator
  implicit val byteTranslator: Translator[Byte] = ByteTranslator
  implicit val charTranslator: Translator[Char] = CharTranslator
  implicit val doubleTranslator: Translator[Double] = DoubleTranslator
  implicit val floatTranslator: Translator[Float] = FloatTranslator
  implicit val intTranslator: Translator[Int] = IntTranslator
  implicit val longTranslator: Translator[Long] = LongTranslator
  implicit val shortTranslator: Translator[Short] = ShortTranslator
  implicit val stringTranslator: Translator[String] = StringTranslator
  implicit def optionTranslator[T](implicit tt: Translator[T]): Translator[Option[T]] = new Translator[Option[T]] {
    override def read(reader: DataReader): Option[T] = if (reader.boolean()) {
      Some(tt.read(reader))
    } else {
      None
    }

    override def write(value: Option[T], writer: DataWriter): DataWriter = value match {
      case Some(t) => tt.write(t, writer.boolean(value = true))
      case None => writer.boolean(value = false)
    }
  }
}