package com.outr.hookup

trait HookupManager {
  def registry: Map[Int, HookupSupport]

  def remote[Interface]: Interface with HookupSupport
  def local[Interface](implementation: Interface): Interface with HookupSupport
  def partial[Interface, Implementation]: Interface with Implementation with HookupSupport
}

trait ServerHookupManager[Manager <: HookupManager] {
  def create(): Manager
}

object HookupManager {
  def server[Manager <: HookupManager]: ServerHookupManager[Manager] = ???
  def create[Manager <: HookupManager]: Manager = ???
}