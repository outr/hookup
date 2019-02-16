package com.outr.hookup

import com.outr.hookup.translate.{MethodCaller, MethodTranslator, Translator}

import scala.annotation.compileTimeOnly
import scala.concurrent.Future
import scala.reflect.macros.blackbox

@compileTimeOnly("Enable Macros for expansion")
object HookupMacros {
  def auto[Interface](context: blackbox.Context)
                     (implicit interface: context.WeakTypeTag[Interface]): context.Expr[AutoHookup[Interface with HookupSupport]] = {
    import context.universe._

    val interfaceClass = interface.tpe.typeSymbol.asClass

    def lookUp(prefix: String): context.Expr[Option[Interface with HookupSupport]] = {
      val className = s"${interfaceClass.owner.fullName}.$prefix${interfaceClass.name.decodedName}"
      val tree = try {
        val clazz = context.mirror.staticClass(className)
        q"Some(${create[Interface](context)(List(clazz.toType), Nil)(interface)})"
      } catch {
        case _: ScalaReflectionException => q"None"
      }
      context.Expr[Option[Interface with HookupSupport]](tree)
    }

    context.Expr[AutoHookup[Interface with HookupSupport]](
      q"""
         import com.outr.hookup._

         AutoHookup[$interface with HookupSupport](${lookUp("Client")}, ${lookUp("Server")})
       """)
  }

  def client[Interface, Implementation](context: blackbox.Context)
                                       (implicit interface: context.WeakTypeTag[Interface],
                                        implementation: context.WeakTypeTag[Implementation]): context.Expr[Interface with Implementation with HookupSupport] = {
    lookupMethods(context)(implementation.tpe, unimplemented = true).foreach { m =>
      val annotations = m.annotations ::: m.overrides.flatMap(_.annotations)
      assert(annotations.exists(_.toString == "com.outr.hookup.server"), s"${m.fullName} is not implemented and isn't annotated as a @server method")
    }

    create[Interface](context)(List(implementation.tpe), Nil)(interface).asInstanceOf[context.Expr[Interface with Implementation with HookupSupport]]
  }

  def server[Interface, Implementation](context: blackbox.Context)
                                       (implicit interface: context.WeakTypeTag[Interface],
                                                 implementation: context.WeakTypeTag[Implementation]): context.Expr[Interface with Implementation with HookupSupport] = {
    lookupMethods(context)(implementation.tpe, unimplemented = true).foreach { m =>
      val annotations = m.annotations ::: m.overrides.flatMap(_.annotations)
      assert(annotations.exists(_.toString == "com.outr.hookup.client"), s"${m.fullName} is not implemented and isn't annotated as a @client method")
    }

    create[Interface](context)(List(implementation.tpe), Nil)(interface).asInstanceOf[context.Expr[Interface with Implementation with HookupSupport]]
  }

  def translator[T](context: blackbox.Context)(t: context.WeakTypeTag[T]): context.Expr[Translator[T]] = {
    import context.universe._
    context.Expr[Translator[T]](
      q"""
         import _root_.com.outr.hookup.data._
         import _root_.com.outr.hookup.translate._
         import _root_.profig.JsonUtil

         new Translator[$t] {
           override def write(value: $t, writer: DataWriter): DataWriter = {
             StringTranslator.write(JsonUtil.toJsonString(value), writer)
           }

           override def read(reader: DataReader): $t = {
             val json = StringTranslator.read(reader)
             JsonUtil.fromJsonString[$t](json)
           }
         }
       """)
  }

  private def lookupMethods(context: blackbox.Context)
                           (tpe: context.universe.Type, unimplemented: Boolean): List[context.Symbol] = {
    import context.universe._

    tpe.members.toList.collect {
      case s if s.isMethod &&
        s.isAbstract == unimplemented &&
        s.asMethod.isPublic &&
        s.typeSignature.resultType <:< typeOf[Future[Any]] => s
    }
  }

  private def abstractMethodByName[I](context: blackbox.Context)
                             (methodName: context.Expr[String], i: context.WeakTypeTag[I]): context.Symbol = {
    import context.universe._

    val methodNameValue = methodName match {
      case Expr(Literal(Constant(value: String))) => value
    }
    lookupMethods(context)(i.tpe, unimplemented = true)
      .find(_.name.decodedName.toString == methodNameValue)
      .getOrElse(context.abort(context.enclosingPosition, s"Unable to find method by name: $methodNameValue"))
  }

  def methodTranslator[I, Params, Result](context: blackbox.Context)
                                         (method: context.Symbol,
                                          i: context.WeakTypeTag[I]): context.Expr[MethodTranslator[Params, Result]] = {
    import context.universe._

    val args = method.typeSignature.paramLists.headOption.getOrElse(Nil)
    val argTypes = args.map(_.typeSignature.resultType)
    val resultType = method.typeSignature.resultType.typeArgs.head
    val writeArgs = argTypes.length match {
      case 0 => List(q"()")
      case 1 => List(q"implicitly[Encoder[${argTypes.head}]].write(value, writer)")
      case _ => argTypes.zipWithIndex.map {
        case (t, index) => {
          val value = TermName(s"_${index + 1}")
          q"implicitly[Encoder[$t]].write(value.$value, writer)"
        }
      }
    }

    context.Expr[MethodTranslator[Params, Result]](q"""
       import _root_.com.outr.hookup._
       import _root_.com.outr.hookup.data._
       import _root_.com.outr.hookup.Hookup._
       import _root_.com.outr.hookup.translate._

       new MethodTranslator[(..$argTypes), $resultType] {
         override val paramsEncoder = new Encoder[(..$argTypes)] {
           override def write(value: (..$argTypes), writer: DataWriter): DataWriter = {
             ..$writeArgs
           }
         }

         override val resultDecoder = implicitly[Decoder[$resultType]]
       }
    """)
  }

  def methodRemote[I, Params, Result](context: blackbox.Context)
                                     (methodName: context.Expr[String])
                                     (implicit i: context.WeakTypeTag[I]): context.Expr[MethodTranslator[Params, Result]] = {
    val method = abstractMethodByName[I](context)(methodName, i)
    methodTranslator[I, Params, Result](context)(method, i)
  }

  def methodCaller[I, Params, Result](context: blackbox.Context)
                                     (method: context.Symbol, implementation: context.Tree)
                                     (implicit i: context.WeakTypeTag[I]): context.Expr[MethodCaller[Params, Result]] = {
    import context.universe._

    val args = method.typeSignature.paramLists.headOption.getOrElse(Nil)
    val argTypes = args.map(_.typeSignature.resultType)
    val resultType = method.typeSignature.resultType.typeArgs.head

    val readArgs = argTypes.length match {
      case 0 => List(q"")
      case 1 => List(q"implicitly[Decoder[${argTypes.head}]].read(reader)")
      case _ => argTypes.map { t =>
        q"implicitly[Decoder[$t]].read(reader)"
      }
    }

    val paramNames = argTypes.zipWithIndex.map {
      case (_, index) => {
        val tn = TermName(s"_${index + 1}")
        q"t.$tn"
      }
    }
    val invoke = argTypes.length match {
      case 0 => q"override def invoke(): Future[$resultType] = $implementation.$method()"
      case 1 => q"override def invoke(value: ${argTypes.head}): Future[$resultType] = $implementation.$method(value)"
      case _ =>
        q"""
           override def invoke(t: (..$argTypes)): Future[$resultType] = {
             $implementation.$method(..$paramNames)
           }
         """
    }

    context.Expr[MethodCaller[Params, Result]](
      q"""
         import _root_.com.outr.hookup._
         import _root_.com.outr.hookup.data._
         import _root_.com.outr.hookup.Hookup._
         import _root_.com.outr.hookup.translate._
         import _root_.scala.concurrent.Future

         new MethodCaller[(..$argTypes), $resultType] {
           override val paramsDecoder = new Decoder[(..$argTypes)] {
             override def read(reader: DataReader): (..$argTypes) = {
               (..$readArgs)
             }
           }

           override val resultEncoder = implicitly[Encoder[$resultType]]

           $invoke
         }
       """)
  }

  def methodLocal[I, Params, Result](context: blackbox.Context)
                                    (methodName: context.Expr[String], implementation: context.Expr[I])
                                    (implicit i: context.WeakTypeTag[I],
                                     p: context.WeakTypeTag[Params],
                                     r: context.WeakTypeTag[Result]): context.Expr[MethodCaller[Params, Result]] = {
    val method = abstractMethodByName[I](context)(methodName, i)
    methodCaller[I, Params, Result](context)(method, implementation.tree)(i)
  }

  def create[I](context: blackbox.Context)
               (interfaces: List[context.Type], implementations: List[context.Tree])
               (implicit i: context.WeakTypeTag[I]): context.Expr[I with HookupSupport] = {
    import context.universe._

    val methods = lookupMethods(context)(i.tpe, unimplemented = true)
    val interfaceMethods = interfaces.flatMap(lookupMethods(context)(_, unimplemented = false))
    val implementationMethodsMap = implementations.flatMap(s => lookupMethods(context)(s.tpe, unimplemented = false).map(_ -> s)).toMap
    val implementationMethods = implementationMethodsMap.keys.toList
    val extraMethods = interfaceMethods ::: implementationMethods

    def isMethodIn(method: Symbol, methods: List[Symbol]): Boolean = {
      val methodName = method.name.toString
      methods.exists { m =>
        m.name.toString == methodName && m.typeSignature.toString == method.typeSignature.toString
      }
    }

    val (implementedMethods, unimplementedMethods) = methods.partition(isMethodIn(_, extraMethods))
    val (methodCallers, definedMethods, callerMapping) = implementedMethods.map { s =>
      val methodName = s.name.toString
      val implMethod = (implementationMethods ::: interfaceMethods).find { es =>
        es.name.toString == methodName && es.typeSignature.toString == s.typeSignature.toString
      }.getOrElse(context.abort(context.enclosingPosition, s"Unable to find defined method: $s"))
      val impl = implementationMethodsMap
        .getOrElse(implMethod, q"self")
      val args = s.typeSignature.paramLists.headOption.getOrElse(Nil)
      val argNames = args.map(_.name.toTermName)
      val argTypes = args.map { arg =>
        arg.typeSignature.resultType
      }
      val params = argNames.zip(argTypes).map {
        case (n, t) => q"$n: $t"
      }
      val callerNameString = s"${methodName}Caller"
      val callerName = TermName(callerNameString)
      val caller = methodCaller(context)(s, impl)(i)
      val definedCaller = q"val $callerName = $caller"
      val definedMethod = if (!isMethodIn(s, interfaceMethods)) {
        q"""
          override def ${s.name.toTermName}(..$params): ${s.typeSignature.resultType} = {
            $impl.${s.name.toTermName}(..$params)
          }
        """
      } else {
        q""
      }
      val mapping = q"$methodName -> $callerName.asInstanceOf[com.outr.hookup.translate.MethodCaller[Any, Any]]"
      (definedCaller, definedMethod, mapping)
    } match {
      case l => (l.map(_._1), l.map(_._2), l.map(_._3))
    }

    val mixIns: List[context.universe.Type] = typeOf[com.outr.hookup.HookupSupport] :: interfaces
    val methodTranslators = unimplementedMethods.map { m =>
      val translatorName = TermName(s"${m.name.decodedName}Translator")
      val translator = methodTranslator(context)(m, i)
      q"val $translatorName = $translator"
    }
    val methodImplementations = unimplementedMethods.map { m =>
      val translatorName = TermName(s"${m.name.decodedName}Translator")
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
           val id = nextId()
           val writer = $translatorName.encode((..$params), createRequestWriter(id, ${m.name.toString}))
           val promise = scala.concurrent.Promise[${m.typeSignature.resultType.typeArgs.head}]
           methodFutures.put(id, reader => {
             val result = $translatorName.decode(reader)
             promise.success(result)
           })
           output := writer

           promise.future
         }
       """
    }

    val expr = context.Expr[I with HookupSupport](
      q"""
         new $i with ..$mixIns { self =>
             override val interfaceName: String = ${i.tpe.typeSymbol.fullName}

             ..$methodCallers

             ..$methodTranslators

             ..$methodImplementations

             override val methodMap = Map[String,com.outr.hookup.translate.MethodCaller[Any,Any]](..$callerMapping)

             ..$definedMethods

             override def hashCode(): Int = interfaceName.hashCode()
           }
         """)
    expr
  }

  def createLocal[I](context: blackbox.Context)
                    (implementation: context.Expr[I])
                    (implicit i: context.WeakTypeTag[I]): context.Expr[I with HookupSupport] = {
    create[I](context)(Nil, List(implementation.tree))(i)
  }

  def createRemote[I](context: blackbox.Context)
                     (implicit i: context.WeakTypeTag[I]): context.Expr[I with HookupSupport] = {
    create[I](context)(Nil, Nil)(i)
  }
}