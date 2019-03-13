package com.outr.hookup

import reactify.Channel

trait HookupIO {
  object io {
    val input: Channel[String] = Channel[String]
    val output: Channel[String] = Channel[String]
  }
}
