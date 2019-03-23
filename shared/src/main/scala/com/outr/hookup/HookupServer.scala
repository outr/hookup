package com.outr.hookup

trait HookupServer[H <: Hookup, Key] {
  private var cache = Map.empty[Key, H]

  def apply(key: Key): H = synchronized {
    cache.get(key) match {
      case Some(h) => h
      case None => {
        val h = create()
        cache += key -> h
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
