package com.outr.hookup

trait HookupServer[H <: Hookup] {
  private var cache = Map.empty[Any, H]
  def map: Map[Any, H] = cache
  def all: List[H] = cache.values.toList

  HookupServer.register(this)

  def apply(key: Any): H = synchronized {
    cache.get(key) match {
      case Some(h) => h
      case None => {
        val h = create(key)
        cache += key -> h
        HookupManager.register(key, h)
        h
      }
    }
  }

  def remove(key: Any): Unit = synchronized {
    cache -= key
  }

  def clear(): Unit = synchronized {
    cache = Map.empty
  }

  protected def create(key: Any): H
}

object HookupServer {
  private var servers = Set.empty[HookupServer[Hookup]]

  private def register[H <: Hookup](server: HookupServer[H]): Unit = synchronized {
    servers += server.asInstanceOf[HookupServer[Hookup]]
  }

  def apply(): Set[HookupServer[Hookup]] = servers

  def clear(): Unit = synchronized {
    servers = Set.empty
  }
}