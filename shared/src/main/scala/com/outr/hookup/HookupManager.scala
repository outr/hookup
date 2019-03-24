package com.outr.hookup

object HookupManager {
  private var _clients = Set.empty[Hookup]
  private var server = Map.empty[Any, Hookup]

  def clients: Set[Hookup] = _clients
  def get[Key](key: Key): Option[Hookup] = server.get(key)
  def apply[Key](key: Key): Hookup = get[Key](key).getOrElse(throw new RuntimeException(s"Not found by key: $key"))

  private[hookup] def register(hookup: Hookup): Unit = synchronized {
    _clients += hookup
  }

  private[hookup] def register[Key](key: Key, hookup: Hookup): Unit = synchronized {
    server += key -> hookup
  }

  private[hookup] def remove(hookup: Hookup): Unit = synchronized {
    if (hookup.isClient) {
      _clients -= hookup
    } else {
      server = server.filterNot(_._2 == hookup)
    }
  }
}