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

import Fingerprint.FingerprintBuilder

/** Configuration of a standard linker. */
final class StandardConfig private (
    /** Scala.js semantics. */
    val semantics: Semantics,
    /** Module kind. */
    val moduleKind: ModuleKind,
    /** How to split modules (if at all). */
    val moduleSplitStyle: ModuleSplitStyle,
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
    /** Name patterns for output. */
    val outputPatterns: OutputPatterns,
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
    val batchMode: Boolean,
    /** The maximum number of (file) writes executed concurrently. */
    val maxConcurrentWrites: Int
) {
  private def this() = {
    this(
        semantics = Semantics.Defaults,
        moduleKind = ModuleKind.NoModule,
        moduleSplitStyle = ModuleSplitStyle.FewestModules,
        esFeatures = ESFeatures.Defaults,
        checkIR = false,
        optimizer = true,
        parallel = true,
        sourceMap = true,
        relativizeSourceMapBase = None,
        outputPatterns = OutputPatterns.Defaults,
        closureCompilerIfAvailable = false,
        prettyPrint = false,
        batchMode = false,
        maxConcurrentWrites = 50
    )
  }

  def withSemantics(semantics: Semantics): StandardConfig =
    copy(semantics = semantics)

  def withSemantics(f: Semantics => Semantics): StandardConfig =
    copy(semantics = f(semantics))

  def withModuleKind(moduleKind: ModuleKind): StandardConfig =
    copy(moduleKind = moduleKind)

  def withModuleSplitStyle(moduleSplitStyle: ModuleSplitStyle): StandardConfig =
    copy(moduleSplitStyle = moduleSplitStyle)

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

  def withOutputPatterns(outputPatterns: OutputPatterns): StandardConfig =
    copy(outputPatterns = outputPatterns)

  def withOutputPatterns(f: OutputPatterns => OutputPatterns): StandardConfig =
    copy(outputPatterns = f(outputPatterns))

  def withClosureCompilerIfAvailable(closureCompilerIfAvailable: Boolean): StandardConfig =
    copy(closureCompilerIfAvailable = closureCompilerIfAvailable)

  def withPrettyPrint(prettyPrint: Boolean): StandardConfig =
    copy(prettyPrint = prettyPrint)

  def withBatchMode(batchMode: Boolean): StandardConfig =
    copy(batchMode = batchMode)

  def withMaxConcurrentWrites(maxConcurrentWrites: Int): StandardConfig =
    copy(maxConcurrentWrites = maxConcurrentWrites)

  override def toString(): String = {
    s"""StandardConfig(
       |  semantics                  = $semantics,
       |  moduleKind                 = $moduleKind,
       |  moduleSplitStyle           = $moduleSplitStyle,
       |  esFeatures                 = $esFeatures,
       |  checkIR                    = $checkIR,
       |  optimizer                  = $optimizer,
       |  parallel                   = $parallel,
       |  sourceMap                  = $sourceMap,
       |  relativizeSourceMapBase    = $relativizeSourceMapBase,
       |  outputPatterns             = $outputPatterns,
       |  closureCompilerIfAvailable = $closureCompilerIfAvailable,
       |  prettyPrint                = $prettyPrint,
       |  batchMode                  = $batchMode,
       |  maxConcurrentWrites        = $maxConcurrentWrites,
       |)""".stripMargin
  }

  private def copy(
      semantics: Semantics = semantics,
      moduleKind: ModuleKind = moduleKind,
      moduleSplitStyle: ModuleSplitStyle = moduleSplitStyle,
      esFeatures: ESFeatures = esFeatures,
      checkIR: Boolean = checkIR,
      optimizer: Boolean = optimizer,
      parallel: Boolean = parallel,
      sourceMap: Boolean = sourceMap,
      outputPatterns: OutputPatterns = outputPatterns,
      relativizeSourceMapBase: Option[URI] = relativizeSourceMapBase,
      closureCompilerIfAvailable: Boolean = closureCompilerIfAvailable,
      prettyPrint: Boolean = prettyPrint,
      batchMode: Boolean = batchMode,
      maxConcurrentWrites: Int = maxConcurrentWrites
  ): StandardConfig = {
    new StandardConfig(
        semantics,
        moduleKind,
        moduleSplitStyle,
        esFeatures,
        checkIR,
        optimizer,
        parallel,
        sourceMap,
        relativizeSourceMapBase,
        outputPatterns,
        closureCompilerIfAvailable,
        prettyPrint,
        batchMode,
        maxConcurrentWrites
    )
  }
}

object StandardConfig {
  import StandardConfigPlatformExtensions.ConfigExt

  private implicit object StandardConfigFingerprint
      extends Fingerprint[StandardConfig] {

    override def fingerprint(config: StandardConfig): String = {
      new FingerprintBuilder("StandardConfig")
        .addField("semantics", config.semantics)
        .addField("moduleKind", config.moduleKind)
        .addField("moduleSplitStyle", config.moduleSplitStyle)
        .addField("esFeatures", config.esFeatures)
        .addField("checkIR", config.checkIR)
        .addField("optimizer", config.optimizer)
        .addField("parallel", config.parallel)
        .addField("sourceMap", config.sourceMap)
        .addField("relativizeSourceMapBase",
            config.relativizeSourceMapBase.map(_.toASCIIString()))
        .addField("outputPatterns", config.outputPatterns)
        .addField("closureCompilerIfAvailable",
            config.closureCompilerIfAvailable)
        .addField("prettyPrint", config.prettyPrint)
        .addField("batchMode", config.batchMode)
        .addField("maxConcurrentWrites", config.maxConcurrentWrites)
        .build()
    }
  }

  def fingerprint(config: StandardConfig): String =
    Fingerprint.fingerprint(config)

  /** Returns the default [[StandardConfig]].
   *
   *  The defaults are:
   *
   *  - `semantics`: [[Semantics.Defaults]]
   *  - `moduleKind`: [[ModuleKind.NoModule]]
   *  - `moduleSplitStyle`: [[ModuleSplitStyle.FewestModules]]
   *  - `esFeatures`: [[ESFeatures.Defaults]]
   *  - `checkIR`: `false`
   *  - `optimizer`: `true`
   *  - `parallel`: `true`
   *  - `sourceMap`: `true`
   *  - `relativizeSourceMapBase`: `None`
   *  - `outputPatterns`: [[OutputPatterns.Defaults]]
   *  - `closureCompilerIfAvailable`: `false`
   *  - `prettyPrint`: `false`
   *  - `batchMode`: `false`
   *  - `maxConcurrentWrites`: `50`
   */
  def apply(): StandardConfig = new StandardConfig()

  implicit def configExt(config: StandardConfig): ConfigExt =
    new ConfigExt(config)
}
