package com.outr.hookup.translate

import com.outr.hookup.data.{DataReader, DataWriter}

trait CoreImplicits {
  implicit val intTranslator: Translator[Int] = IntTranslator
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