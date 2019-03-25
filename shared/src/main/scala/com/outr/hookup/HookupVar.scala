package com.outr.hookup

import io.circe.{Decoder, Encoder, Json}
import reactify.Var
import reactify.standard.StandardVar

class HookupVar[T](initialValue: => T,
                   hookup: Hookup,
                   name: String,
                   encoder: Encoder[T],
                   decoder: Decoder[T]) extends StandardVar[T](initialValue, Var.Mode.Normal, Some(name)) {
  private val firing = new ThreadLocal[Boolean] {
    override def initialValue(): Boolean = false
  }

  val tuple: (String, Json => Unit) = name -> ((json: Json) => {
    val value = decoder.decodeJson(json) match {
      case Left(failure) => throw new RuntimeException(s"Failed to decode from $json", failure)
      case Right(v) => v
    }
    firing.set(true)
    try {
      set(value)
    } finally {
      firing.remove()
    }
  })

  attach { value =>
    if (!firing.get()) {
      val json = encoder(value)
      hookup.io.output := HookupSupport.channel(0L, name, json)
    }
  }
}
