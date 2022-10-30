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

package org.scalajs.nscplugin

import scala.tools.nsc._
import scala.tools.nsc.plugins.{
  Plugin => NscPlugin, PluginComponent => NscPluginComponent
}
import scala.collection.{ mutable, immutable }

import java.net.{ URI, URISyntaxException }

import org.scalajs.ir.Trees

/** Main entry point for the Scala.js compiler plugin
 *
 *  @author Sébastien Doeraene
 */
class ScalaJSPlugin(val global: Global) extends NscPlugin {
  import global._

  val name = "scalajs"
  val description = "Compile to JavaScript"
  val components = {
    if (global.isInstanceOf[doc.ScaladocGlobal]) {
      List[NscPluginComponent](PrepInteropComponent)
    } else {
      List[NscPluginComponent](PreTyperComponentComponent, PrepInteropComponent,
          ExplicitInnerJSComponent, ExplicitLocalJSComponent, GenCodeComponent)
    }
  }

  /** Called when the JS ASTs are generated. Override for testing */
  def generatedJSAST(clDefs: List[Trees.ClassDef]): Unit = {}

  /** A trick to avoid early initializers while still enforcing that `global`
   *  is initialized early.
   */
  abstract class JSGlobalAddonsEarlyInit[G <: Global with Singleton](val global: G)
      extends JSGlobalAddons

  /** Addons for the JavaScript platform. */
  object jsAddons extends JSGlobalAddonsEarlyInit[global.type](global)

  object scalaJSOpts extends ScalaJSOptions {
    import ScalaJSOptions.URIMap
    var fixClassOf: Boolean = false
    var genStaticForwardersForNonTopLevelObjects: Boolean = false
    lazy val sourceURIMaps: List[URIMap] = {
      if (_sourceURIMaps.nonEmpty)
        _sourceURIMaps.reverse
      else
        relSourceMap.toList.map(URIMap(_, absSourceMap))
    }
    var warnGlobalExecutionContext: Boolean = true
    var _sourceURIMaps: List[URIMap] = Nil
    var relSourceMap: Option[URI] = None
    var absSourceMap: Option[URI] = None
  }

  object PreTyperComponentComponent extends PreTyperComponent(global) {
    val runsAfter = List("parser")
    override val runsBefore = List("namer")
  }

  object PrepInteropComponent extends PrepJSInterop[global.type](global) {
    val jsAddons: ScalaJSPlugin.this.jsAddons.type = ScalaJSPlugin.this.jsAddons
    val scalaJSOpts = ScalaJSPlugin.this.scalaJSOpts
    override val runsAfter = List("typer")
    override val runsBefore = List("pickle")
  }

  object ExplicitInnerJSComponent extends ExplicitInnerJS[global.type](global) {
    val jsAddons: ScalaJSPlugin.this.jsAddons.type = ScalaJSPlugin.this.jsAddons
    override val runsAfter = List("refchecks")
    override val runsBefore = List("uncurry")
  }

  object ExplicitLocalJSComponent extends ExplicitLocalJS[global.type](global) {
    val jsAddons: ScalaJSPlugin.this.jsAddons.type = ScalaJSPlugin.this.jsAddons
    override val runsAfter = List("specialize")
    override val runsBefore = List("explicitouter")
  }

  object GenCodeComponent extends GenJSCode[global.type](global) {
    val jsAddons: ScalaJSPlugin.this.jsAddons.type = ScalaJSPlugin.this.jsAddons
    val scalaJSOpts = ScalaJSPlugin.this.scalaJSOpts
    override val runsAfter = List("mixin")
    override val runsBefore = List("delambdafy", "cleanup", "terminal")

    def generatedJSAST(clDefs: List[Trees.ClassDef]): Unit =
      ScalaJSPlugin.this.generatedJSAST(clDefs)
  }

  override def init(options: List[String], error: String => Unit): Boolean = {
    import ScalaJSOptions.URIMap
    import scalaJSOpts._

    for (option <- options) {
      if (option == "fixClassOf") {
        fixClassOf = true
      } else if (option == "genStaticForwardersForNonTopLevelObjects") {
        genStaticForwardersForNonTopLevelObjects = true
      } else if (option == "nowarnGlobalExecutionContext") {
        warnGlobalExecutionContext = false
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

    true // this plugin is always enabled
  }

  override val optionsHelp: Option[String] = Some(s"""
      |  -P:$name:mapSourceURI:FROM_URI[->TO_URI]
      |     Change the location the source URIs in the emitted IR point to
      |     - strips away the prefix FROM_URI (if it matches)
      |     - optionally prefixes the TO_URI, where stripping has been performed
      |     - any number of occurrences are allowed. Processing is done on a first match basis.
      |  -P:$name:genStaticForwardersForNonTopLevelObjects
      |     Generate static forwarders for non-top-level objects.
      |     This option should be used by codebases that implement JDK classes.
      |     When used together with -Xno-forwarders, this option has no effect.
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
