/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.tools.nsc._
import scala.tools.nsc.plugins.{
  Plugin => NscPlugin, PluginComponent => NscPluginComponent
}
import scala.collection.{ mutable, immutable }

import java.net.{ URI, URISyntaxException }

/** Main entry point for the Scala.js compiler plugin
 *
 *  @author Sébastien Doeraene
 */
class ScalaJSPlugin(val global: Global) extends NscPlugin {
  import global._

  val name = "scalajs"
  val description = "Compile to JavaScript"
  val components = List[NscPluginComponent](PrepInteropComponent, GenCodeComponent)

  /** Called when the JS ASTs are generated. Override for testing */
  def generatedJSAST(clDefs: List[jsAddons.js.Tree]): Unit = {}

  /** Addons for JavaScript platform */
  object jsAddons extends {
    val global: ScalaJSPlugin.this.global.type = ScalaJSPlugin.this.global
  } with JSGlobalAddons with Compat210Component

  object scalaJSOpts extends ScalaJSOptions {
    var fixClassOf:   Boolean     = false
    var noSourceMap:  Boolean     = false
    var relSourceMap: Option[URI] = None
    var absSourceMap: Option[URI] = None
  }

  object PrepInteropComponent extends {
    val global: ScalaJSPlugin.this.global.type = ScalaJSPlugin.this.global
    val jsAddons: ScalaJSPlugin.this.jsAddons.type = ScalaJSPlugin.this.jsAddons
    val scalaJSOpts = ScalaJSPlugin.this.scalaJSOpts
    override val runsAfter = List("typer")
    override val runsBefore = List("pickle")
  } with PrepJSInterop

  object GenCodeComponent extends {
    val global: ScalaJSPlugin.this.global.type = ScalaJSPlugin.this.global
    val jsAddons: ScalaJSPlugin.this.jsAddons.type = ScalaJSPlugin.this.jsAddons
    val scalaJSOpts = ScalaJSPlugin.this.scalaJSOpts
    override val runsAfter = List("mixin")
    override val runsBefore = List("delambdafy", "cleanup", "terminal")
  } with GenJSCode {
    def generatedJSAST(clDefs: List[jsAddons.js.Tree]) =
      ScalaJSPlugin.this.generatedJSAST(clDefs)
  }

  override def processOptions(options: List[String],
      error: String => Unit): Unit = {
    import scalaJSOpts._

    for (option <- options) {
      if (option == "fixClassOf") {
        fixClassOf = true

      } else if (option == "noSourceMap") {
        noSourceMap = true
      } else if (option.startsWith("relSourceMap:")) {
        val uriStr = option.stripPrefix("relSourceMap:")
        try { relSourceMap = Some(new URI(uriStr)) }
        catch {
          case e: URISyntaxException => error(s"$uriStr is not a valid URI")
        }
      } else if (option.startsWith("absSourceMap:")) {
        val uriStr = option.stripPrefix("absSourceMap:")
        try { absSourceMap = Some(new URI(uriStr)) }
        catch {
          case e: URISyntaxException => error(s"$uriStr is not a valid URI")
        }
      } else {
        error("Option not understood: " + option)
      }
    }

    // Verfiy constraits on flags that require others
    if (absSourceMap.isDefined && relSourceMap.isEmpty)
      error("absSourceMap requires the use of relSourceMap")
  }

  override val optionsHelp: Option[String] = Some(s"""
      |  -P:$name:noSourceMap         turn off source map generation
      |  -P:$name:relSourceMap:<URI>  relativize emitted source maps with <URI>
      |  -P:$name:absSourceMap:<URI>  absolutize emitted source maps with <URI>
            This option requires the use of relSourceMap
      |  -P:$name:fixClassOf          repair calls to Predef.classOf that reach ScalaJS
      |     WARNING: This is a tremendous hack! Expect ugly errors if you use this option.
      """.stripMargin)

}
