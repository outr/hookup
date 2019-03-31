package com.outr.hookup

trait HookupServer[H <: Hookup, Key] {
  private var cache = Map.empty[Key, H]

  HookupServer.register(this)

  def apply(key: Key): H = synchronized {
    cache.get(key) match {
      case Some(h) => h
      case None => {
        val h = create()
        cache += key -> h
        HookupManager.register(key, h)
        h
      }
    }
  }

  def remove(key: Key): Unit = synchronized {
    cache -= key
  }

  def clear(): Unit = synchronized {
    cache = Map.empty
  }

  protected def create(): H
}

object HookupServer {
  private var servers = Set.empty[HookupServer[Hookup, Any]]

  private def register[H <: Hookup, Key](server: HookupServer[H, Key]): Unit = synchronized {
    servers += server.asInstanceOf[HookupServer[Hookup, Any]]
  }

  def apply(): Set[HookupServer[Hookup, Any]] = servers

  def clear(): Unit = synchronized {
    servers = Set.empty
  }
}