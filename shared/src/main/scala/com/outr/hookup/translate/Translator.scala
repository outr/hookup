package com.outr.hookup.translate

trait Translator[Value] extends Encoder[Value] with Decoder[Value]