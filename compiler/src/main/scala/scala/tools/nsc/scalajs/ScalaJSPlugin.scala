package scala.tools.nsc
package scalajs

import scala.tools.nsc.plugins.{
  Plugin => NscPlugin, PluginComponent => NscPluginComponent
}
import scala.collection.{ mutable, immutable }

class ScalaJSPlugin(val global: Global) extends NscPlugin {
  import global._

  val name = "scalajs"
  val description = "Compile to JavaScript"
  val components = List[NscPluginComponent](PluginComponent)

  /** Addons for JavaScript platform */
  object jsAddons extends {
    val global: ScalaJSPlugin.this.global.type = ScalaJSPlugin.this.global
  } with JSGlobalAddons

  object PluginComponent extends {
    val global: ScalaJSPlugin.this.global.type = ScalaJSPlugin.this.global
    val jsAddons: ScalaJSPlugin.this.jsAddons.type = ScalaJSPlugin.this.jsAddons
    override val runsAfter = List("mixin")
    override val runsBefore = List("cleanup", "terminal")
  } with backend.js.GenJSCode
}
