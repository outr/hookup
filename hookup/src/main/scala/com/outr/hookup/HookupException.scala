package com.outr.hookup

case class HookupException(message: String, errorType: String, cause: Option[Throwable] = None) extends RuntimeException(s"$message ($errorType)", cause.orNull)