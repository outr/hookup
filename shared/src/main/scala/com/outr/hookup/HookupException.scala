package com.outr.hookup

case class HookupException(message: String, errorType: String) extends RuntimeException(message)