package com.outr.hookup

import scala.util.Random

object HookupManager {
  private val clientId = Random.nextLong()
  private var map = Map.empty[Any, Hookups]

  def clients: Hookups = apply(clientId)
  def get[Key](key: Key): Option[Hookups] = map.get(key)
  def apply[Key](key: Key, registerAllServers: Boolean = false): Hookups = synchronized {
    if (registerAllServers) {
      HookupServer().foreach(s => s(key))
    }
    scribe.debug(s"Hooking up: $key = ${get(key)}")
    get(key) match {
      case Some(h) => h
      case None => {
        val h = new Hookups
        map += key -> h
        h
      }
    }
  }

  private[hookup] def register(hookup: Hookup): Unit = synchronized {
    apply(clientId) += hookup
  }

  private[hookup] def register[Key](key: Key, hookup: Hookup): Unit = synchronized {
    apply(key) += hookup
  }

  private[hookup] def remove(hookup: Hookup): Unit = synchronized {
    if (hookup.isClient) {
      apply(clientId) -= hookup
    } else {
      map.values.foreach(_ -= hookup)
    }
  }

  def remove(hookups: Hookups): Unit = synchronized {
    hookups.clear()
    map -= hookups
  }

  def clear(): Unit = synchronized {
    map.values.foreach(_.clear())
    map = Map.empty
  }
}