package com.outr.hookup

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

trait HookupManager extends HookupIO {
  private var map = Map.empty[Int, HookupSupport]
  protected def register[I <: HookupSupport](i: I): I = synchronized {
    i.output.attach { writer =>
      output := writer.int(i.hashCode(), prepend = true)
    }
    map += i.hashCode() -> i
    i
  }
  protected def register[I <: HookupSupport](auto: AutoHookup[I]): I = register(if (isServer) {
    auto.server.getOrElse(throw new RuntimeException(s"No server implementation found!"))
  } else {
    auto.client.getOrElse(throw new RuntimeException(s"No client implementation found!"))
  })

  input.attach { reader =>
    val id = reader.int()
    val hookupSupport = map.getOrElse(id, throw new RuntimeException(s"Unable to find HookupSupport instance by id: $id"))
    hookupSupport.input := reader
  }

  def isServer: Boolean
  def isClient: Boolean = !isServer

  def registry: Map[Int, HookupSupport] = map
}

trait ServerHookupManager[Manager <: HookupManager] {
  def create(): Manager
}

object HookupManager {
  def client[Manager <: HookupManager]: Manager = macro clientMacro[Manager]
  def server[Manager <: HookupManager]: ServerHookupManager[Manager] = macro serverMacro[Manager]

  def createMacro[Manager <: HookupManager](context: blackbox.Context)
                                           (server: Boolean, m: context.WeakTypeTag[Manager]): context.Expr[Manager] = {
    import context.universe._

    context.Expr[Manager](
      q"""
         new $m {
           override def isServer: Boolean = $server
         }
       """)
  }

  def clientMacro[Manager <: HookupManager](context: blackbox.Context)
                                           (implicit m: context.WeakTypeTag[Manager]): context.Expr[Manager] = {
    createMacro[Manager](context)(server = false, m = m)
  }

  def serverMacro[Manager <: HookupManager](context: blackbox.Context)
                                           (implicit m: context.WeakTypeTag[Manager]): context.Expr[ServerHookupManager[Manager]] = {
    import context.universe._

    val create = createMacro[Manager](context)(server = true, m = m)

    context.Expr[ServerHookupManager[Manager]](
      q"""
         new com.outr.hookup.ServerHookupManager[$m] {
           override def create(): $m = $create
         }
       """)
  }
}