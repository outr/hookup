package hookup

import scala.concurrent.Future
import scala.reflect.macros.blackbox

object HookupMacros {
  def interface[Interface](context: blackbox.Context)
                          (implicit interface: context.WeakTypeTag[Interface]): context.Expr[Hookup[Interface]] = {
    import context.universe._

    val methods = lookupMethods(context)(interface.tpe)
    val (remote, local) = methods.partition(_.isAbstract)

    scribe.info(s"Local: $local, Remote: $remote")
    val remoteEntries = remote.map { s =>
      val key = s.name.decodedName.toString
      scribe.info(s"KEY: $key")
      val params = s.typeSignature.paramLists.flatten.map { p =>
        val name = p.name.toString
        val `type` = p.typeSignature.resultType
        scribe.info(s"Param: $name: ${`type`}")
      }
    }

    context.abort(context.enclosingPosition, "Not finished")
  }

  def implementation[Interface](context: blackbox.Context)
                               (implementation: context.Expr[Interface])
                               (implicit interface: context.WeakTypeTag[Interface]): context.Expr[Hookup[Interface]] = {
    import context.universe._

    context.abort(context.enclosingPosition, "Not finished")
  }

  private def lookupMethods(context: blackbox.Context)
                           (tpe: context.universe.Type): List[context.Symbol] = {
    import context.universe._

    tpe.members.toList.collect {
      case s if s.isMethod && s.asMethod.isPublic && s.typeSignature.resultType <:< typeOf[Future[Any]] => s
    }
  }
}