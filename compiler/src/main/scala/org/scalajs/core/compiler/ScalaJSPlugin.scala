/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
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
  object jsAddons extends {
    val global: ScalaJSPlugin.this.global.type = ScalaJSPlugin.this.global
  } with JSGlobalAddons with Compat210Component

  object scalaJSOpts extends ScalaJSOptions {
    import ScalaJSOptions.URIMap
    var fixClassOf: Boolean = false
    var suppressExportDeprecations: Boolean = false
    var suppressMissingJSGlobalDeprecations: Boolean = false
    lazy val sourceURIMaps: List[URIMap] = {
      if (_sourceURIMaps.nonEmpty)
        _sourceURIMaps.reverse
      else
        relSourceMap.toList.map(URIMap(_, absSourceMap))
    }
    var _sourceURIMaps: List[URIMap] = Nil
    var relSourceMap: Option[URI] = None
    var absSourceMap: Option[URI] = None
    var sjsDefinedByDefault: Boolean = false
  }

  /** Checks and registers module exports on the symbol.
   *
   *  This bridge allows other plugins to register new modules for export
   *  between jsinterop and jscode phases. It is meant to be accessed using
   *  reflection. The calling code still must insert the `@JSExport` annotation
   *  to the module.
   */
  @deprecated("Might be removed at any time, use at your own risk.", "0.6.24")
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
      } else if (option == "suppressExportDeprecations") {
        suppressExportDeprecations = true
      } else if (option == "suppressMissingJSGlobalDeprecations") {
        suppressMissingJSGlobalDeprecations = true
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
      } else if (option == "sjsDefinedByDefault") {
        sjsDefinedByDefault = true
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
      |     Change the location the source URIs in the emitted IR point to
      |     - strips away the prefix FROM_URI (if it matches)
      |     - optionally prefixes the TO_URI, where stripping has been performed
      |     - any number of occurences are allowed. Processing is done on a first match basis.
      |  -P:$name:suppressExportDeprecations
      |     Silence deprecations of top-level @JSExport,
      |     @JSExportDescendentClasses and @JSExportDescendentObjects.
      |     This can be used as a transition path in the 0.6.x cycle,
      |     to avoid too many deprecation warnings that are not trivial
      |     to address.
      |  -P:$name:fixClassOf
      |     Repair calls to Predef.classOf that reach Scala.js.
      |     WARNING: This is a tremendous hack! Expect ugly errors if you use this option.
      |Deprecated options
      |  -P:$name:relSourceMap:<URI>
      |     Relativize emitted source maps with <URI>
      |  -P:$name:absSourceMap:<URI>
      |     Absolutize emitted source maps with <URI>
      |     This option requires the use of relSourceMap
      """.stripMargin)

}
