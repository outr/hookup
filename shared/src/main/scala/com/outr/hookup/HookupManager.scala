package com.outr.hookup

import java.nio.ByteBuffer

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

trait HookupManager extends HookupIO {
  private var map = Map.empty[Int, HookupSupport]
  protected def register(hs: HookupSupport): Unit = synchronized {
    hs.output.attach { bb =>
      val wrapper = ByteBuffer.allocate(bb.)
    }
    map += hs.hashCode() -> hs
  }

  input.attach { bb =>
    val id = bb.getInt
    val hookupSupport = map.getOrElse(id, throw new RuntimeException(s"Unable to find HookupSupport instance by id: $id"))
    hookupSupport.input := bb
  }

  def registry: Map[Int, HookupSupport] = map

  def remote[Interface]: Interface with HookupSupport = ???
  def local[Interface](implementation: Interface): Interface with HookupSupport = ???
  def partial[Interface, Implementation]: Interface with Implementation with HookupSupport = ???
  def auto[Interface]: Interface with HookupSupport = ???
}

trait ServerHookupManager[Manager <: HookupManager] {
  def create(): Manager
}

object HookupManager {
  def create[Manager <: HookupManager]: Manager = macro createMacro[Manager]
  def server[Manager <: HookupManager]: ServerHookupManager[Manager] = macro serverMacro[Manager]

  def createMacro[Manager <: HookupManager](context: blackbox.Context)
                                           (m: context.WeakTypeTag[Manager]): context.Expr[Manager] = {
    import context.universe._

    context.abort(context.enclosingPosition, "Not implemented")
  }

  def serverMacro[Manager <: HookupManager](context: blackbox.Context)
                                           (m: context.WeakTypeTag[Manager]): context.Expr[ServerHookupManager[Manager]] = {
    import context.universe._

    context.Expr[ServerHookupManager[Manager]](
      q"""
         new com.outr.hookup.ServerHookupManager[$m] {
           override def create(): $m = com.outr.hookup.HookupManager.create[$m]
         }
       """)
  }
}