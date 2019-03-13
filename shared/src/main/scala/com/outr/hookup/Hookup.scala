package com.outr.hookup

import scala.concurrent.Future
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object Hookup {
  def apply[I]: HookupManager[I] = macro simple[I]
  def apply[I, T]: HookupManager[T] = macro oneInterface[I, T]
  def apply[I](implementation: I): HookupManager[I] = macro oneImplementation[I]

  def simple[I](context: blackbox.Context)(implicit i: context.WeakTypeTag[I]): context.Expr[HookupManager[I]] = {
    manager[I](context)(i, Nil, Nil)
  }

  def oneInterface[I, T](context: blackbox.Context)
                        (implicit i: context.WeakTypeTag[I],
                         t: context.WeakTypeTag[T]): context.Expr[HookupManager[I]] = {
    manager[I](context)(i, List(t.tpe), Nil)
  }

  def oneImplementation[I](context: blackbox.Context)
                          (implementation: context.Expr[I])
                          (implicit i: context.WeakTypeTag[I]): context.Expr[HookupManager[I]] = {
    manager[I](context)(i, Nil, List(implementation.tree))
  }

  def manager[I](context: blackbox.Context)
                (i: context.WeakTypeTag[I],
                 interfaces: List[context.Type],
                 implementations: List[context.Tree]): context.Expr[HookupManager[I]] = {
    import context.universe._

    def sig(s: Symbol): String = {
      s"${s.name}:${s.typeSignature.toString}"
    }

    val methods = lookupMethods(context)(i.tpe, unimplemented = None)
    val abstractMethods = methods.filter(_.isAbstract)
    val concreteMethods = methods.filterNot(_.isAbstract)
    val interfaceMethods = interfaces.flatMap(lookupMethods(context)(_, unimplemented = Some(false)))
    val implementationMethodsMap = implementations.flatMap(s => lookupMethods(context)(s.tpe, unimplemented = Some(false)).map(_ -> s)).toMap
    val implementationMethods = implementationMethodsMap.keys.toList
    val extraMethods = interfaceMethods ::: implementationMethods
    val unimplementedMethods = abstractMethods.filterNot { s =>
      val methodSignature = sig(s)
      extraMethods.exists(sig(_) == methodSignature)
    }
    val mixIns: List[context.universe.Type] = typeOf[com.outr.hookup.HookupSupport] :: interfaces

    val remoteMethods = unimplementedMethods.map()

    context.Expr[HookupManager[I]](
      q"""
         import _root_.com.outr.hookup._

         new HookupManager[$i] {
           override def create(): $i with HookupSupport = new $i with ..$mixIns {
             override val interfaceName: String = ${i.tpe.typeSymbol.fullName}

             // Register methods implemented locally that can be invoked remotely
             ..$registerLocalMethods

             // Methods not implemented locally, so they must be invoked remotely
             ..$remoteMethods

             // Methods not implemented in the interface, but implemented via an implementation
             ..$implementationMethods

             override def hashCode(): Int = interfaceName.hashCode()
           }
         }
       """)
  }

  private def lookupMethods(context: blackbox.Context)
                           (tpe: context.universe.Type, unimplemented: Option[Boolean]): List[context.Symbol] = {
    import context.universe._

    tpe.members.toList.collect {
      case s if s.isMethod &&
        unimplemented.forall(s.isAbstract == _) &&
        s.asMethod.isPublic &&
        s.typeSignature.resultType <:< typeOf[Future[Any]] => s
    }
  }
}