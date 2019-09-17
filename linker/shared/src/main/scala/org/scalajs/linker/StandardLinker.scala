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

package org.scalajs.linker

import scala.language.implicitConversions

import java.net.URI

import org.scalajs.linker.standard._

object StandardLinker {
  import StandardLinkerPlatformExtensions._

  def apply(config: Config): Linker = {
    StandardLinkerImpl(StandardLinkerFrontend(config),
        StandardLinkerBackend(config))
  }

  def clearable(config: Config): ClearableLinker =
    ClearableLinker(() => apply(config), config.batchMode)

  implicit def configExt(config: Config): ConfigExt =
    new ConfigExt(config)

  /** Configuration of a standard linker. */
  final class Config private (
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

    def withSemantics(semantics: Semantics): Config =
      copy(semantics = semantics)

    def withSemantics(f: Semantics => Semantics): Config =
      copy(semantics = f(semantics))

    def withModuleKind(moduleKind: ModuleKind): Config =
      copy(moduleKind = moduleKind)

    def withESFeatures(esFeatures: ESFeatures): Config =
      copy(esFeatures = esFeatures)

    def withESFeatures(f: ESFeatures => ESFeatures): Config =
      copy(esFeatures = f(esFeatures))

    def withCheckIR(checkIR: Boolean): Config =
      copy(checkIR = checkIR)

    def withOptimizer(optimizer: Boolean): Config =
      copy(optimizer = optimizer)

    def withParallel(parallel: Boolean): Config =
      copy(parallel = parallel)

    def withSourceMap(sourceMap: Boolean): Config =
      copy(sourceMap = sourceMap)

    def withRelativizeSourceMapBase(relativizeSourceMapBase: Option[URI]): Config =
      copy(relativizeSourceMapBase = relativizeSourceMapBase)

    def withClosureCompilerIfAvailable(closureCompilerIfAvailable: Boolean): Config =
      copy(closureCompilerIfAvailable = closureCompilerIfAvailable)

    def withPrettyPrint(prettyPrint: Boolean): Config =
      copy(prettyPrint = prettyPrint)

    def withBatchMode(batchMode: Boolean): Config =
      copy(batchMode = batchMode)

    override def toString(): String = {
      s"""StandardLinker.Config(
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
    ): Config = {
      new Config(
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

  object Config {
    /** Returns the default configuration for a [[StandardLinker]].
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
    def apply(): Config = new Config()
  }

}
