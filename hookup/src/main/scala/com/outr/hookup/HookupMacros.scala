package com.outr.hookup

import reactify.{Channel, Var}

import scala.annotation.compileTimeOnly
import scala.concurrent.Future
import scala.reflect.macros.blackbox

@compileTimeOnly("Enable Macros for expansion")
object HookupMacros {
  def createClient[H <: Hookup](context: blackbox.Context)(implicit h: context.WeakTypeTag[H]): context.Expr[H] = {
    import context.universe._

    context.Expr[H](
      q"""
         new $h {
           override def isClient: Boolean = true
         }
       """)
  }

  def createServer[H <: Hookup, Key](context: blackbox.Context)
                                    (implicit h: context.WeakTypeTag[H], key: context.WeakTypeTag[Key]): context.Expr[HookupServer[H, Key]] = {
    import context.universe._

    context.Expr[HookupServer[H, Key]](
      q"""
         import _root_.com.outr.hookup._

         new HookupServer[$h, $key] {
           override def create(): $h = new $h {
             override def isClient: Boolean = false
           }
         }
       """)
  }

  def auto[I](context: blackbox.Context)(implicit i: context.WeakTypeTag[I]): context.Expr[I with HookupSupport] = {
    import context.universe._

    val instance = context.prefix.tree
    val packageName = findPackage(context)(i.tpe.typeSymbol)
    val classes = context
      .mirror
      .staticPackage(packageName)
      .typeSignature
      .decls
      .filter(_.isClass)
      .filter(_.typeSignature.baseClasses.contains(i.tpe.typeSymbol))
      .toList
      .flatMap { s =>
        s.typeSignature.baseClasses.collect {
          case c if c.typeSignature.baseClasses.contains(i.tpe.typeSymbol) && c != i.tpe.typeSymbol => c
        }
      }
    val clientInterface = classes.find(_.fullName.toLowerCase.contains("client"))
    val serverInterface = classes.find(_.fullName.toLowerCase.contains("server"))

    def verifyAnnotation(interface: Option[context.Symbol], annotationName: String): Unit = {
      interface.foreach { c =>
        c.typeSignature.members.toList.foreach {
          case symbol if symbol.isMethod && symbol.asMethod.isPublic && symbol.typeSignature.resultType <:< typeOf[Future[Any]] => {
            val annotations = symbol.annotations ::: symbol.overrides.flatMap(_.annotations)
            val isAnnotated = annotations.exists(_.toString == annotationName)
            if (isAnnotated && symbol.isAbstract) {
              context.abort(context.enclosingPosition, s"${symbol.fullName} is not implemented in ${c.fullName}")
            }
          }
          case _ =>
        }
      }
    }

    verifyAnnotation(clientInterface, "com.outr.hookup.client")
    verifyAnnotation(serverInterface, "com.outr.hookup.server")

    def notImplemented = context.Expr[I with HookupSupport](q"""throw new RuntimeException("No implementation found!")""")
    val clientImplementation = clientInterface.map(t => create[I](context)(i, List(t.asClass.selfType), Nil)).getOrElse(notImplemented)
    val serverImplementation = serverInterface.map(t => create[I](context)(i, List(t.asClass.selfType), Nil)).getOrElse(notImplemented)
    context.Expr[I with HookupSupport](
      q"""
         $instance.register(if ($instance.isClient) {
           $clientImplementation
         } else {
           $serverImplementation
         })
       """)
  }

  private def findPackage(context: blackbox.Context)(s: context.Symbol): String = if (s.isPackage) {
    s.fullName
  } else {
    findPackage(context)(s.owner)
  }

  def simple[I](context: blackbox.Context)(implicit i: context.WeakTypeTag[I]): context.Expr[I with HookupSupport] = {
    import context.universe._
    val instance = context.prefix.tree
    val expr = create[I](context)(i, Nil, Nil)
    context.Expr[I with HookupSupport](q"$instance.register($expr)")
  }

  def oneInterface[I, T](context: blackbox.Context)
                        (implicit i: context.WeakTypeTag[I], t: context.WeakTypeTag[T]): context.Expr[I with HookupSupport] = {
    import context.universe._
    val instance = context.prefix.tree
    val expr = create[I](context)(i, List(t.tpe), Nil)
    context.Expr[I with HookupSupport](q"$instance.register($expr)")
  }

  def oneImplementation[I](context: blackbox.Context)
                          (implementation: context.Expr[I])
                          (implicit i: context.WeakTypeTag[I]): context.Expr[I with HookupSupport] = {
    import context.universe._
    val instance = context.prefix.tree
    val expr = create[I](context)(i, Nil, List(implementation.tree))
    context.Expr[I with HookupSupport](q"$instance.register($expr)")
  }

  def channel[I](context: blackbox.Context)(i: context.WeakTypeTag[I]): context.Expr[Channel[I]] = {
    import context.universe._

    val name = context.freshName("channel")
    val channel = q"""
       import _root_.com.outr.hookup._
       import _root_.io.circe._
       import _root_.io.circe.generic.auto._
       import _root_.io.circe.syntax._
       import _root_.reactify.Channel

       val channel = new HookupChannel[$i](
         hookup = ${context.prefix.tree},
         name = $name,
         encoder = implicitly[Encoder[$i]],
         decoder = implicitly[Decoder[$i]]
       )
       channels += channel.tuple
       channel
     """
    context.Expr[Channel[I]](channel)
  }

  def prop[I](context: blackbox.Context)
             (initialValue: context.Tree)
             (i: context.WeakTypeTag[I]): context.Expr[Var[I]] = {
    import context.universe._

    val name = context.freshName("channel")
    val p = q"""
       import _root_.com.outr.hookup._
       import _root_.io.circe._
       import _root_.io.circe.generic.auto._
       import _root_.io.circe.syntax._
       import _root_.reactify.Channel

       val channel = new HookupVar[$i](
         initialValue = $initialValue,
         hookup = ${context.prefix.tree},
         name = $name,
         encoder = implicitly[Encoder[$i]],
         decoder = implicitly[Decoder[$i]]
       )
       channels += channel.tuple
       channel
     """
    context.Expr[Var[I]](p)
  }

  def create[I](context: blackbox.Context)
               (i: context.WeakTypeTag[I],
                interfaces: List[context.Type],
                implementations: List[context.Tree]): context.Expr[I with HookupSupport] = {
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
    val concreteMethods = methods.filterNot(_.isAbstract) ::: interfaceMethods
    val unimplementedMethods = abstractMethods.filterNot { s =>
      val methodSignature = sig(s)
      extraMethods.exists(sig(_) == methodSignature)
    }
    val mixIns: List[context.universe.Type] = typeOf[com.outr.hookup.HookupSupport] :: interfaces

    val interfaceName: String = i.tpe.typeSymbol.fullName

    def createCallable(m: context.Symbol, useThis: Boolean): context.Tree = {
      val args = m.typeSignature.paramLists.headOption.getOrElse(Nil)
      val argTypes = args.map { arg =>
        arg.typeSignature.resultType
      }
      val argNames = args.map(_.name.toTermName)
      val prefix = if (useThis) q"this.$m" else q"$m"
      val call = argNames.zip(argTypes).map {
        case (n, t) =>
          val name = n.decodedName.toString
          q"""
             $n = (implicitly[Decoder[$t]].decodeJson((json \\ $name).head) match {
               case Left(failure) => throw new RuntimeException("Failed to decode from $$response", failure)
               case Right(value) => value
             })
           """
      }
      val invoke = q"$prefix(..$call)"
      val name = s"$interfaceName.${m.name}"
      q"""
        ($name, HookupCallable(${m.name.toString}, json => {
          $invoke.map(result => result.asJson)
        }))
       """
    }

    val callables = concreteMethods.map(m => createCallable(m, useThis = true)) :::
      implementationMethods.map(m => createCallable(m, useThis = false))

    val remoteMethods = unimplementedMethods.map { m =>
      val args = m.typeSignature.paramLists.headOption.getOrElse(Nil)
      val argNames = args.map(_.name.toTermName)
      val argTypes = args.map { arg =>
        arg.typeSignature.resultType
      }
      val jsonify = argNames.map { n =>
        q"${n.decodedName.toString} -> $n.asJson"
      }
      val params = argNames.zip(argTypes).map {
        case (n, t) => q"$n: $t"
      }
      q"""
         override def ${m.name.toTermName}(..$params): ${m.typeSignature.resultType} = {
           val params: _root_.io.circe.Json = Json.obj(..$jsonify)
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

    val hookup = q"""
       import _root_.com.outr.hookup._
       import _root_.io.circe._
       import _root_.io.circe.generic.auto._
       import _root_.io.circe.syntax._

       new $i with ..$mixIns {
         override val interfaceName: String = $interfaceName

         // Callables
         override val callables: Map[String, HookupCallable] = Map(..$callables)

         // Methods not implemented locally, so they must be invoked remotely
         ..$remoteMethods

         // Methods not implemented in the interface, but implemented via an implementation
         ..$implementedMethods

         override def hashCode(): Int = interfaceName.hashCode()
       }
     """
    context.Expr[I with HookupSupport](hookup)
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