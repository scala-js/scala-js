/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.backend

import scala.language.implicitConversions

import java.net.URI

import org.scalajs.core.tools.io.WritableVirtualJSFile
import org.scalajs.core.tools.javascript.ESLevel
import org.scalajs.core.tools.logging.Logger
import org.scalajs.core.tools.sem.Semantics

import org.scalajs.core.tools.linker.LinkingUnit
import org.scalajs.core.tools.linker.analyzer.SymbolRequirement

/** A backend of the Scala.js linker. Produces a
 *  [[org.scalajs.core.tools.io.VirtualJSFile VirtualJSFile]].
 *
 *  You probably want to use an instance of [[linker.Linker]], rather than this
 *  low-level class.
 */
abstract class LinkerBackend(
    val semantics: Semantics,
    val esLevel: ESLevel,
    val moduleKind: ModuleKind,
    protected val config: LinkerBackend.Config) {

  @deprecated(
      "Use the overload without 'withSourceMap'. " +
      "The parameter can be configured in the 'config'.",
      "0.6.17")
  def this(semantics: Semantics, esLevel: ESLevel, moduleKind: ModuleKind,
      withSourceMap: Boolean, config: LinkerBackend.Config) = {
    this(semantics, esLevel, moduleKind, config.withSourceMap(withSourceMap))
  }

  @deprecated("Use the overload with an explicit ModuleKind", "0.6.13")
  def this(semantics: Semantics, esLevel: ESLevel, withSourceMap: Boolean,
      config: LinkerBackend.Config) {
    this(semantics, esLevel, ModuleKind.NoModule, withSourceMap, config)
  }

  @deprecated("Use config.sourceMap.", "0.6.17")
  val withSourceMap: Boolean = config.sourceMap

  /** Symbols this backend needs to be present in the linking unit. */
  val symbolRequirements: SymbolRequirement

  /** Emit the given [[LinkingUnit]] to the target output
   *
   *  @param unit [[LinkingUnit]] to emit
   *  @param output File to write to
   *  @param logger Logger to use
   */
  def emit(unit: LinkingUnit, output: WritableVirtualJSFile,
      logger: Logger): Unit

  /** Verify that a [[LinkingUnit]] corresponds to this [[LinkerBackend]]'s
   *  [[org.scalajs.core.tools.sem.Semantics Semantics]] and
   *  [[org.scalajs.core.tools.javascript.ESLevel ESLevel]] (specified via the
   *  [[org.scalajs.core.tools.javascript.OutputMode OutputMode]]).
   *  @throws java.lang.IllegalArgumentException if there is a mismatch
   */
  protected def verifyUnit(unit: LinkingUnit): Unit = {
    require(unit.semantics == semantics,
        "LinkingUnit and LinkerBackend must agree on semantics")
    require(unit.esLevel == esLevel,
        "LinkingUnit and LinkerBackend must agree on esLevel")
  }
}

object LinkerBackend extends LinkerBackendPlatformExtensions {
  /** Configurations relevant to the backend */
  final class Config private (
      /** Whether to emit a source map. */
      val sourceMap: Boolean = true,
      /** Base path to relativize paths in the source map. */
      val relativizeSourceMapBase: Option[URI] = None,
      /** Custom js code that wraps the output */
      val customOutputWrapper: (String, String) = ("", ""),
      /** Whether to use the Google Closure Compiler pass, if it is available.
       *  On the JavaScript platform, this does not have any effect.
       */
      val closureCompilerIfAvailable: Boolean = false,
      /** Pretty-print the output. */
      val prettyPrint: Boolean = false
  ) {
    def withSourceMap(sourceMap: Boolean): Config =
      copy(sourceMap = sourceMap)

    def withRelativizeSourceMapBase(relativizeSourceMapBase: Option[URI]): Config =
      copy(relativizeSourceMapBase = relativizeSourceMapBase)

    @deprecated(
        "The functionality of custom output wrappers has been superseded " +
        "by the support for CommonJS modules, module initializers, and " +
        "top-level exports.",
        "0.6.15")
    def withCustomOutputWrapper(customOutputWrapper: (String, String)): Config =
      copy(customOutputWrapper = customOutputWrapper)

    // Non-deprecated version to call from the sbt plugin
    private[scalajs] def withCustomOutputWrapperInternal(
        customOutputWrapper: (String, String)): Config = {
      copy(customOutputWrapper = customOutputWrapper)
    }

    def withClosureCompilerIfAvailable(closureCompilerIfAvailable: Boolean): Config =
      copy(closureCompilerIfAvailable = closureCompilerIfAvailable)

    def withPrettyPrint(prettyPrint: Boolean): Config =
      copy(prettyPrint = prettyPrint)

    private def copy(
        sourceMap: Boolean = sourceMap,
        relativizeSourceMapBase: Option[URI] = relativizeSourceMapBase,
        customOutputWrapper: (String, String) = customOutputWrapper,
        closureCompilerIfAvailable: Boolean = closureCompilerIfAvailable,
        prettyPrint: Boolean = prettyPrint): Config = {
      new Config(sourceMap, relativizeSourceMapBase, customOutputWrapper,
          closureCompilerIfAvailable, prettyPrint)
    }
  }

  object Config {
    import LinkerBackendPlatformExtensions._

    implicit def toPlatformExtensions(config: Config): ConfigExt =
      new ConfigExt(config)

    def apply(): Config = new Config()
  }
}
