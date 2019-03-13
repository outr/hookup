package com.outr.hookup

trait HookupManager[I] {
  def create(): I with HookupSupport
}
