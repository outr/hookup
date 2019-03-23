package com.outr.hookup

case class HookupManager[I](interfaceName: String, create: () => I with HookupSupport)