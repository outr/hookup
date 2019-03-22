package com.outr.hookup

trait HookupManager[I] {
  def interfaceName: String
  def create(): I with HookupSupport
}
