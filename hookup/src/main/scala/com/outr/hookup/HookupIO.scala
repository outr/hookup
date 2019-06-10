package com.outr.hookup

import io.circe.Json
import reactify.Channel

trait HookupIO {
  object io {
    val input: Channel[Json] = Channel[Json]
    val output: Channel[Json] = Channel[Json]
  }
}
