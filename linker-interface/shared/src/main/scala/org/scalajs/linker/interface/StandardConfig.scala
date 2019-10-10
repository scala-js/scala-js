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

package org.scalajs.linker.interface

import scala.language.implicitConversions

import java.net.URI

/** Configuration of a standard linker. */
final class StandardConfig private (
    /** Scala.js semantics. */
    val semantics: Semantics,
    /** Module kind. */
    val moduleKind: ModuleKind,
    /** ECMAScript features to use. */
    val esFeatures: ESFeatures,
    /** If true, performs expensive checks of the IR for the used parts. */
    val checkIR: Boolean,
    /** Whether to use the Scala.js optimizer. */
    val optimizer: Boolean,
    /** Whether things that can be parallelized should be parallelized.
     *  On the JavaScript platform, this does not have any effect.
     */
    val parallel: Boolean,
    /** Whether to emit a source map. */
    val sourceMap: Boolean,
    /** Base path to relativize paths in the source map. */
    val relativizeSourceMapBase: Option[URI],
    /** Whether to use the Google Closure Compiler pass, if it is available.
     *  On the JavaScript platform, this does not have any effect.
     */
    val closureCompilerIfAvailable: Boolean,
    /** Pretty-print the output. */
    val prettyPrint: Boolean,
    /** Whether the linker should run in batch mode.
     *
     *  In batch mode, the linker can throw away intermediate state that is
     *  otherwise maintained for incremental runs.
     *
     *  This setting is only a hint. A linker and/or its subcomponents may
     *  ignore it. This applies in both directions: a linker not supporting
     *  incrementality can ignore `batchMode = false`, and a contrario, a
     *  linker mainly designed for incremental runs may ignore
     *  `batchMode = true`.
     */
    val batchMode: Boolean
) {
  private def this() = {
    this(
        semantics = Semantics.Defaults,
        moduleKind = ModuleKind.NoModule,
        esFeatures = ESFeatures.Defaults,
        checkIR = true,
        optimizer = true,
        parallel = true,
        sourceMap = true,
        relativizeSourceMapBase = None,
        closureCompilerIfAvailable = false,
        prettyPrint = false,
        batchMode = false
    )
  }

  def withSemantics(semantics: Semantics): StandardConfig =
    copy(semantics = semantics)

  def withSemantics(f: Semantics => Semantics): StandardConfig =
    copy(semantics = f(semantics))

  def withModuleKind(moduleKind: ModuleKind): StandardConfig =
    copy(moduleKind = moduleKind)

  def withESFeatures(esFeatures: ESFeatures): StandardConfig =
    copy(esFeatures = esFeatures)

  def withESFeatures(f: ESFeatures => ESFeatures): StandardConfig =
    copy(esFeatures = f(esFeatures))

  def withCheckIR(checkIR: Boolean): StandardConfig =
    copy(checkIR = checkIR)

  def withOptimizer(optimizer: Boolean): StandardConfig =
    copy(optimizer = optimizer)

  def withParallel(parallel: Boolean): StandardConfig =
    copy(parallel = parallel)

  def withSourceMap(sourceMap: Boolean): StandardConfig =
    copy(sourceMap = sourceMap)

  def withRelativizeSourceMapBase(relativizeSourceMapBase: Option[URI]): StandardConfig =
    copy(relativizeSourceMapBase = relativizeSourceMapBase)

  def withClosureCompilerIfAvailable(closureCompilerIfAvailable: Boolean): StandardConfig =
    copy(closureCompilerIfAvailable = closureCompilerIfAvailable)

  def withPrettyPrint(prettyPrint: Boolean): StandardConfig =
    copy(prettyPrint = prettyPrint)

  def withBatchMode(batchMode: Boolean): StandardConfig =
    copy(batchMode = batchMode)

  override def toString(): String = {
    s"""StandardConfig(
       |  semantics                  = $semantics,
       |  moduleKind                 = $moduleKind,
       |  esFeatures                 = $esFeatures,
       |  checkIR                    = $checkIR,
       |  optimizer                  = $optimizer,
       |  parallel                   = $parallel,
       |  sourceMap                  = $sourceMap,
       |  relativizeSourceMapBase    = $relativizeSourceMapBase,
       |  closureCompilerIfAvailable = $closureCompilerIfAvailable,
       |  prettyPrint                = $prettyPrint,
       |  batchMode                  = $batchMode,
       |)""".stripMargin
  }

  private def copy(
      semantics: Semantics = semantics,
      moduleKind: ModuleKind = moduleKind,
      esFeatures: ESFeatures = esFeatures,
      checkIR: Boolean = checkIR,
      optimizer: Boolean = optimizer,
      parallel: Boolean = parallel,
      sourceMap: Boolean = sourceMap,
      relativizeSourceMapBase: Option[URI] = relativizeSourceMapBase,
      closureCompilerIfAvailable: Boolean = closureCompilerIfAvailable,
      prettyPrint: Boolean = prettyPrint,
      batchMode: Boolean = batchMode
  ): StandardConfig = {
    new StandardConfig(
        semantics,
        moduleKind,
        esFeatures,
        checkIR,
        optimizer,
        parallel,
        sourceMap,
        relativizeSourceMapBase,
        closureCompilerIfAvailable,
        prettyPrint,
        batchMode
    )
  }
}

object StandardConfig {
  import StandardConfigPlatformExtensions.ConfigExt

  /** Returns the default [[StandardConfig]].
   *
   *  The defaults are:
   *
   *  - `semantics`: [[Semantics.Defaults]]
   *  - `moduleKind`: [[ModuleKind.NoModule]]
   *  - `esFeatures`: [[ESFeatures.Defaults]]
   *  - `checkIR`: `true`
   *  - `optimizer`: `true`
   *  - `parallel`: `true`
   *  - `sourceMap`: `true`
   *  - `relativizeSourceMapBase`: `None`
   *  - `closureCompilerIfAvailable`: `false`
   *  - `prettyPrint`: `false`
   *  - `batchMode`: `false`
   */
  def apply(): StandardConfig = new StandardConfig()

  implicit def configExt(config: StandardConfig): ConfigExt =
    new ConfigExt(config)
}
