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

package org.scalajs.core.tools.linker.backend

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
    val withSourceMap: Boolean,
    protected val config: LinkerBackend.Config) {

  @deprecated("Use the overload with an explicit ModuleKind", "0.6.13")
  def this(semantics: Semantics, esLevel: ESLevel, withSourceMap: Boolean,
      config: LinkerBackend.Config) = {
    this(semantics, esLevel, ModuleKind.NoModule, withSourceMap, config)
  }

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

object LinkerBackend {
  /** Configurations relevant to the backend */
  final class Config private (
      /** Base path to relativize paths in the source map. */
      val relativizeSourceMapBase: Option[URI] = None,
      /** Custom js code that wraps the output */
      val customOutputWrapper: (String, String) = ("", ""),
      /** Pretty-print the output. */
      val prettyPrint: Boolean = false
  ) {
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

    def withPrettyPrint(prettyPrint: Boolean): Config =
      copy(prettyPrint = prettyPrint)

    private def copy(
        relativizeSourceMapBase: Option[URI] = relativizeSourceMapBase,
        customOutputWrapper: (String, String) = customOutputWrapper,
        prettyPrint: Boolean = prettyPrint): Config = {
      new Config(relativizeSourceMapBase, customOutputWrapper, prettyPrint)
    }
  }

  object Config {
    def apply(): Config = new Config()
  }
}
