/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package org.scalajs.core.compiler

import scala.tools.nsc._
import scala.tools.nsc.plugins.{
  Plugin => NscPlugin, PluginComponent => NscPluginComponent
}
import scala.collection.{ mutable, immutable }

import java.net.{ URI, URISyntaxException }

import org.scalajs.core.ir.Trees

/** Main entry point for the Scala.js compiler plugin
 *
 *  @author Sébastien Doeraene
 */
class ScalaJSPlugin(val global: Global) extends NscPlugin {
  import global._

  val name = "scalajs"
  val description = "Compile to JavaScript"
  val components = {
    if (global.forScaladoc) {
      List[NscPluginComponent](PrepInteropComponent)
    } else {
      List[NscPluginComponent](PreTyperComponentComponent, PrepInteropComponent,
          GenCodeComponent)
    }
  }

  /** Called when the JS ASTs are generated. Override for testing */
  def generatedJSAST(clDefs: List[Trees.Tree]): Unit = {}

  /** Addons for JavaScript platform */
  object jsAddons extends { // scalastyle:ignore
    val global: ScalaJSPlugin.this.global.type = ScalaJSPlugin.this.global
  } with JSGlobalAddons with Compat210Component

  object scalaJSOpts extends ScalaJSOptions { // scalastyle:ignore
    import ScalaJSOptions.URIMap
    var fixClassOf: Boolean = false
    lazy val sourceURIMaps: List[URIMap] = {
      if (_sourceURIMaps.nonEmpty)
        _sourceURIMaps.reverse
      else
        relSourceMap.toList.map(URIMap(_, absSourceMap))
    }
    var _sourceURIMaps: List[URIMap] = Nil
    var relSourceMap: Option[URI] = None
    var absSourceMap: Option[URI] = None
  }

  /** Checks and registers module exports on the symbol.
   *  This bridge allows other plugins (such as ScalaJSJUnitPlugin) to register
   *  new modules for export between jsinterop and jscode phases. It is meant to
   *  be accessed using reflection. The calling code still must insert the
   *  `@JSExport` annotation to the module.
   */
  def registerModuleExports(sym: Symbol): Unit =
    PrepInteropComponent.registerModuleExports(sym)

  object PreTyperComponentComponent extends {
    val global: ScalaJSPlugin.this.global.type = ScalaJSPlugin.this.global
    val runsAfter = List("parser")
    override val runsBefore = List("namer")
  } with PreTyperComponent

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
    def generatedJSAST(clDefs: List[Trees.Tree]): Unit =
      ScalaJSPlugin.this.generatedJSAST(clDefs)
  }

  override def processOptions(options: List[String],
      error: String => Unit): Unit = {
    import ScalaJSOptions.URIMap
    import scalaJSOpts._

    for (option <- options) {
      if (option == "fixClassOf") {
        fixClassOf = true

      } else if (option.startsWith("mapSourceURI:")) {
        val uris = option.stripPrefix("mapSourceURI:").split("->")

        if (uris.length != 1 && uris.length != 2) {
          error("relocateSourceMap needs one or two URIs as argument.")
        } else {
          try {
            val from = new URI(uris.head)
            val to = uris.lift(1).map(str => new URI(str))
            _sourceURIMaps ::= URIMap(from, to)
          } catch {
            case e: URISyntaxException =>
              error(s"${e.getInput} is not a valid URI")
          }
        }

      // The following options are deprecated (how do we show this to the user?)
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

    // Verify constraints
    if (_sourceURIMaps.nonEmpty && relSourceMap.isDefined)
      error("You may not use mapSourceURI and relSourceMap together. " +
          "Use another mapSourceURI option without second URI.")
    else if (_sourceURIMaps.nonEmpty && absSourceMap.isDefined)
      error("You may not use mapSourceURI and absSourceMap together. " +
          "Use another mapSourceURI option.")
    else if (absSourceMap.isDefined && relSourceMap.isEmpty)
      error("absSourceMap requires the use of relSourceMap")
  }

  override val optionsHelp: Option[String] = Some(s"""
      |  -P:$name:mapSourceURI:FROM_URI[->TO_URI]
      |     change the location the source URIs in the emitted IR point to
      |     - strips away the prefix FROM_URI (if it matches)
      |     - optionally prefixes the TO_URI, where stripping has been performed
      |     - any number of occurences are allowed. Processing is done on a first match basis.
      |  -P:$name:fixClassOf          repair calls to Predef.classOf that reach ScalaJS
      |     WARNING: This is a tremendous hack! Expect ugly errors if you use this option.
      |Deprecated options
      |  -P:$name:relSourceMap:<URI>  relativize emitted source maps with <URI>
      |  -P:$name:absSourceMap:<URI>  absolutize emitted source maps with <URI>
      |     This option requires the use of relSourceMap
      """.stripMargin)

}
