package com.outr.hookup

import io.circe.Json
import reactify.reaction.Reaction

import scala.util.Random

object HookupManager {
  private val clientId = Random.nextLong()
  private var map = Map.empty[Any, Hookups]

  def clients: Hookups = apply(clientId)
  def get[Key](key: Key): Option[Hookups] = map.get(key)
  def apply[Key](key: Key): Hookups = synchronized {
    scribe.info(s"Hooking up: $key = ${get(key)}")
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

  def clear(): Unit = synchronized {
    map.values.foreach(_.clear())
    map = Map.empty
  }
}

class Hookups extends HookupIO {
  private var entries = Set.empty[Hookup]
  private val outputReaction = Reaction[Json](json => {
    io.output := json
  })

  // Receive input
  io.input.attach { json =>
    val name = (json \\ "name").head.asString.get
    val interfaceName = name.substring(0, name.lastIndexOf('.'))
    scribe.info(s"Receiving input for $interfaceName (${entries.map(_.callables.keySet)})")
    val hookup = entries.find(_.hasInterface(interfaceName)).getOrElse(throw new RuntimeException(s"No Hookup found for $interfaceName"))
    hookup.io.input := json
  }

  def +=(hookup: Hookup): Unit = synchronized {
    entries += hookup
    hookup.io.output.reactions += outputReaction
  }

  def -=(hookup: Hookup): Unit = synchronized {
    entries -= hookup
    hookup.io.output.reactions -= outputReaction
  }

  def clear(): Unit = synchronized {
    entries.foreach(_.io.output.reactions -= outputReaction)
    entries = Set.empty
  }
}