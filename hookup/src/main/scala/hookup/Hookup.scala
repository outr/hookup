package hookup

import io.circe.Json

import scala.concurrent.Future
import scala.language.experimental.macros

case class Hookup[Interface](local: Map[String, Json => Future[Json]], remote: Map[String, Json => Json])

object Hookup {
  def apply[Interface]: Hookup[Interface] = macro HookupMacros.interface[Interface]
  def apply[Interface](implementation: Interface): Hookup[Interface] = macro HookupMacros.implementation[Interface]
}