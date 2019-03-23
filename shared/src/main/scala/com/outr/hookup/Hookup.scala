package com.outr.hookup

import scala.language.experimental.macros

trait Hookup extends HookupIO {
  private var _callables = Map.empty[String, HookupIO]
  def callables: Map[String, HookupIO] = _callables

  def isClient: Boolean
  def isServer: Boolean = !isClient

  io.input.attach { json =>
    val method = (json \\ "name").head.asString.get
    val interfaceName = method.substring(0, method.lastIndexOf('.'))
    callables.get(interfaceName) match {
      case Some(hookupIO) => hookupIO.io.input := json
      case None => throw new RuntimeException(s"No interface mapping found for: $interfaceName ($json) (keys: ${callables.keySet})")
    }
  }

  protected def register[I](i: I with HookupSupport): I with HookupSupport = synchronized {
    i.io.output.attach(json => io.output := json)
    _callables += i.interfaceName -> i
    i
  }

  protected def create[I]: I with HookupSupport = macro HookupMacros.simple[I]
  protected def create[I, T]: I with HookupSupport = macro HookupMacros.oneInterface[I, T]
  protected def create[I](implementation: I): I with HookupSupport = macro HookupMacros.oneImplementation[I]
  protected def auto[I]: I with HookupSupport = macro HookupMacros.auto[I]
}

object Hookup {
  def client[H <: Hookup]: H = macro HookupMacros.createClient[H]
  def server[H <: Hookup, Key]: HookupServer[H, Key] = macro HookupMacros.createServer[H, Key]

  object connect {
    def direct(first: HookupIO, second: HookupIO): Unit = {
      first.io.output.attach { json =>
        second.io.input := json
      }
      second.io.output.attach { json =>
        first.io.input := json
      }
    }
  }
}