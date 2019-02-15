package com.outr.hookup

import java.nio.ByteBuffer

import reactify.Channel

trait HookupIO {
  /**
    * Called when external requests are made to invoke a local method or when a response is coming back from a remote
    * method invocation.
    *
    * This channel is monitored internally.
    */
  val input: Channel[ByteBuffer] = Channel[ByteBuffer]

  /**
    * Called when a local method interface is invoked that must be executed remotely.
    *
    * This channel should be monitored by the external implementation to transfer to the input of the remote interface.
    */
  val output: Channel[ByteBuffer] = Channel[ByteBuffer]
}
