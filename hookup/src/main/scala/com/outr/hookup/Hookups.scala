package com.outr.hookup

import io.circe.Json
import reactify.reaction.Reaction

class Hookups extends HookupIO {
  private var _entries = Set.empty[Hookup]
  private val outputReaction = Reaction[Json](json => {
    io.output := json
  })

  def entries: Set[Hookup] = _entries

  // Receive input
  io.input.attach { json =>
    val name = (json \\ "name").head.asString.get
    val interfaceName = name.substring(0, name.lastIndexOf('.'))
    scribe.debug(s"Receiving input for $interfaceName (${_entries.map(_.callables.keySet)})")
    val hookup = _entries.find(_.hasInterface(interfaceName)).getOrElse(throw new RuntimeException(s"No Hookup found for $interfaceName"))
    hookup.io.input := json
  }

  def +=(hookup: Hookup): Unit = synchronized {
    _entries += hookup
    hookup.io.output.reactions += outputReaction
  }

  def -=(hookup: Hookup): Unit = synchronized {
    _entries -= hookup
    hookup.io.output.reactions -= outputReaction
  }

  def clear(): Unit = synchronized {
    _entries.foreach(_.io.output.reactions -= outputReaction)
    _entries = Set.empty
  }
}