package com.outr.hookup

import com.outr.hookup.translate.{CoreImplicits, MethodCaller, MethodTranslator, Translator}

import scala.language.experimental.macros

object Hookup extends CoreImplicits {
  object Action {
    val MethodRequest: Byte = 1.toByte
    val MethodResponse: Byte = 2.toByte
  }

  def translator[T]: Translator[T] = macro HookupMacros.translator[T]

  def method[I, Params, Result](methodName: String): MethodTranslator[Params, Result] = macro HookupMacros.methodRemote[I, Params, Result]
  def method[I, Params, Result](methodName: String, implementation: I): MethodCaller[Params, Result] = macro HookupMacros.methodLocal[I, Params, Result]
  def create[I]: I with HookupSupport = macro HookupMacros.createRemote[I]
  def create[I](implementation: I): I with HookupSupport = macro HookupMacros.createLocal[I]
  def client[Interface, Implementation]: Interface with Implementation with HookupSupport = macro HookupMacros.client[Interface, Implementation]
  def server[Interface, Implementation]: Interface with Implementation with HookupSupport = macro HookupMacros.server[Interface, Implementation]

  object connect {
    def direct(first: HookupSupport, second: HookupSupport): Unit = {
      first.output.attach { bb =>
        second.input := bb
      }
      second.output.attach { bb =>
        first.input := bb
      }
    }
  }
}