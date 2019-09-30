package com.outr.hookup

import io.circe.Json
import reactify.{Channel, Var}

import scala.language.experimental.macros

trait Hookup extends HookupIO {
  private var _callables = Map.empty[String, HookupSupport]
  def callables: Map[String, HookupSupport] = _callables

  protected var channels = Map.empty[String, Json => Unit]

  def key: Any
  def keyAs[Key]: Key = key.asInstanceOf[Key]

  def isClient: Boolean
  def isServer: Boolean = !isClient

  def hasInterface(interfaceName: String): Boolean = callables.contains(interfaceName)

  if (isClient) {
    HookupManager.register(this)
  }

  io.input.attach { json =>
    val `type` = (json \\ "type").head.asString.get
    val name = (json \\ "name").head.asString.get
    `type` match {
      case HookupSupport.`type`.Channel => {
        val name = (json \\ "name").head.asString.get
        val value = (json \\ "value").head
        channels.get(name) match {
          case Some(caller) => {
            try {
              caller(value)
            } catch {
              case t: Throwable => scribe.error(s"Error while invoking callable $name", t)
            }
          }
          case None => {
            val message = s"No callable channel found for: $name"
            scribe.error(message)
          }
        }
      }
      case _ => {
        val interfaceName = name.substring(0, name.lastIndexOf('.'))
        callables.get(interfaceName) match {
          case Some(hookupIO) => hookupIO.io.input := json
          case None => throw new RuntimeException(s"No interface mapping found for: $interfaceName ($json) (keys: ${callables.keySet})")
        }
      }
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
  protected def channel[I]: Channel[I] = macro HookupMacros.channel[I]
  protected def prop[I](initialValue: => I): Var[I] = macro HookupMacros.prop[I]

  def dispose(): Unit = {
    HookupManager.remove(this)
    callables.values.toList.distinct.foreach(_.dispose())
  }
}

object Hookup {
  val error: Channel[Throwable] = Channel[Throwable]

  object connect {
    def queue(hookup: HookupIO): HookupQueue = {
      val queue = new HookupQueue
      hookup.io.output.attach(queue += _)
      queue
    }
    def direct(first: HookupIO, second: HookupIO): Disconnectable = {
      val r1 = first.io.output.attach { json =>
        second.io.input := json
      }
      val r2 = second.io.output.attach { json =>
        first.io.input := json
      }
      new Disconnectable {
        override def disconnect(): Unit = {
          first.io.output.reactions -= r1
          second.io.output.reactions -= r2
        }
      }
    }
    def log(hios: (String, HookupIO)*): Unit = hios.foreach {
      case (name, hio) => {
        hio.io.input.attach { json =>
          scribe.info(s"[$name] ${hio.getClass.getSimpleName} input: $json")
        }
        hio.io.output.attach { json =>
          scribe.info(s"[$name] ${hio.getClass.getSimpleName} output: $json")
        }
      }
    }
  }
}