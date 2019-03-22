package com.outr.hookup

// TODO: implement functionality
trait HookupServer[H <: Hookup, Key] {
  def apply(key: Key): H = create()

  protected def create(): H
}
