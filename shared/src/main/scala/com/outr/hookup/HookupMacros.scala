package com.outr.hookup

import scala.annotation.compileTimeOnly
import scala.concurrent.Future
import scala.reflect.macros.blackbox

@compileTimeOnly("Enable Macros for expansion")
object HookupMacros {
  def createClient[H <: Hookup](context: blackbox.Context)(implicit h: context.WeakTypeTag[H]): context.Expr[H] = {
    import context.universe._

    context.Expr[H](q"new $h {}")
  }

  def createServer[H <: Hookup, Key](context: blackbox.Context)
                                    (implicit h: context.WeakTypeTag[H], key: context.WeakTypeTag[Key]): context.Expr[HookupServer[H, Key]] = {
    import context.universe._

    context.Expr[HookupServer[H, Key]](
      q"""
         import _root_.com.outr.hookup._

         new HookupServer[$h, $key] {
           override def create(): $h = new $h {}
         }
       """)
  }

  def instanceSimple[I](context: blackbox.Context)(implicit i: context.WeakTypeTag[I]): context.Expr[I with HookupSupport] = {
    import context.universe._
    val instance = context.prefix.tree
    val expr = simple[I](context)(i)
    context.Expr[I with HookupSupport](q"$instance.register($expr.create())")
  }

  def instanceOneImplementation[I](context: blackbox.Context)
                                  (implementation: context.Expr[I])
                                  (implicit i: context.WeakTypeTag[I]): context.Expr[I with HookupSupport] = {
    import context.universe._
    val instance = context.prefix.tree
    val expr = oneImplementation[I](context)(implementation)(i)
    context.Expr[I with HookupSupport](q"$instance.register($expr.create())")
  }

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
    val interfaceMethods = interfaces.flatMap(lookupMethods(context)(_, unimplemented = Some(false)))
    val implementationMethodsMap = implementations.flatMap(s => lookupMethods(context)(s.tpe, unimplemented = Some(false)).map(_ -> s)).toMap
    val implementationMethods = implementationMethodsMap.keys.toList
    val extraMethods = interfaceMethods ::: implementationMethods
    val concreteMethods = methods.filterNot(_.isAbstract) ::: interfaceMethods ::: implementationMethods
    val unimplementedMethods = abstractMethods.filterNot { s =>
      val methodSignature = sig(s)
      extraMethods.exists(sig(_) == methodSignature)
    }
    val mixIns: List[context.universe.Type] = typeOf[com.outr.hookup.HookupSupport] :: interfaces

    val callables = concreteMethods.map { m =>
      val args = m.typeSignature.paramLists.headOption.getOrElse(Nil)
      val argTypes = args.map { arg =>
        arg.typeSignature.resultType
      }
      val params = args.zipWithIndex.map {
        case (_, index) => q"params.${TermName(s"_${index + 1}")}"
      }
      val invoke = if (argTypes.isEmpty) {
        q"$m()"
      } else if (argTypes.tail.isEmpty) {
        q"""
           val param: ${argTypes.head} = implicitly[Decoder[${argTypes.head}]].decodeJson(json) match {
             case Left(failure) => throw new RuntimeException("Failed to decode from $$response", failure)
             case Right(value) => value
           }
           $m(param)
         """
      } else {
        q"""
           val params: (..$argTypes) = implicitly[Decoder[(..$argTypes)]].decodeJson(json) match {
             case Left(failure) => throw new RuntimeException("Failed to decode from $$response", failure)
             case Right(value) => value
           }
           $m(..$params)
         """
      }
      q"""
        new HookupCallable {
          override val name: String = ${m.name.toString}
          override def call(json: Json): Future[Json] = {
            $invoke.map(result => result.asJson)
          }
        }
       """
    }

    val remoteMethods = unimplementedMethods.map { m =>
      val args = m.typeSignature.paramLists.headOption.getOrElse(Nil)
      val argNames = args.map(_.name.toTermName)
      val argTypes = args.map { arg =>
        arg.typeSignature.resultType
      }
      val params = argNames.zip(argTypes).map {
        case (n, t) => q"$n: $t"
      }
      q"""
         override def ${m.name.toTermName}(..$params): ${m.typeSignature.resultType} = {
           val params: _root_.io.circe.Json = (..$argNames).asJson
           remoteInvoke(${m.fullName}, params).map { response =>
             implicitly[Decoder[${m.typeSignature.resultType.typeArgs.head}]].decodeJson(response) match {
               case Left(failure) => throw new RuntimeException("Failed to decode from $$response", failure)
               case Right(value) => value
             }
           }
         }
       """
    }

    val implementedMethods = implementationMethods.map { m =>
      val args = m.typeSignature.paramLists.headOption.getOrElse(Nil)
      val argNames = args.map(_.name.toTermName)
      val argTypes = args.map { arg =>
        arg.typeSignature.resultType
      }
      val params = argNames.zip(argTypes).map {
        case (n, t) => q"$n: $t"
      }
      val implementation = implementationMethodsMap(m)
      q"""
         override def ${m.name.toTermName}(..$params): ${m.typeSignature.resultType} = {
           $implementation.${m.name.toTermName}(..$argNames)
         }
       """
    }

    val manager = q"""
       import _root_.com.outr.hookup._
       import _root_.io.circe._
       import _root_.io.circe.generic.auto._
       import _root_.io.circe.syntax._

       new HookupManager[$i] {
         override val interfaceName: String = ${i.tpe.typeSymbol.fullName}

         override def create(): $i with HookupSupport = new $i with ..$mixIns {
           override val interfaceName: String = ${i.tpe.typeSymbol.fullName}

           // Callables
           override val callables: Map[String, HookupCallable] = List[HookupCallable](..$callables).map(c => s"$$interfaceName.$${c.name}" -> c).toMap

           // Methods not implemented locally, so they must be invoked remotely
           ..$remoteMethods

           // Methods not implemented in the interface, but implemented via an implementation
           ..$implementedMethods

           override def hashCode(): Int = interfaceName.hashCode()
         }
       }
     """
    context.Expr[HookupManager[I]](manager)
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