/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.core.tools.linker

import scala.language.implicitConversions

import java.net.URI

import org.scalajs.core.tools.linker.standard._
import org.scalajs.core.tools.linker.frontend.LinkerFrontend
import org.scalajs.core.tools.linker.backend.LinkerBackend

object StandardLinker {
  import StandardLinkerPlatformExtensions._

  def apply(config: Config): Linker = {
    val coreSpec = CoreSpec(
        config.semantics,
        config.moduleKind,
        config.outputMode)

    val commonConfig = CommonPhaseConfig()
      .withCoreSpec(coreSpec)
      .withParallel(config.parallel)
      .withBatchMode(config.batchMode)

    val frontendConfig = LinkerFrontend.Config()
      .withCommonConfig(commonConfig)
      .withCheckIR(config.checkIR)
      .withOptimizer(config.optimizer)

    val backendConfig = LinkerBackend.Config()
      .withCommonConfig(commonConfig)
      .withSourceMap(config.sourceMap)
      .withRelativizeSourceMapBase(config.relativizeSourceMapBase)
      .withClosureCompilerIfAvailable(config.closureCompilerIfAvailable)
      .withPrettyPrint(config.prettyPrint)

    Linker(LinkerFrontend(frontendConfig), LinkerBackend(backendConfig))
  }

  implicit def configExt(config: Config): ConfigExt =
    new ConfigExt(config)

  /** Configuration of a standard linker. */
  final class Config private (
      /** Scala.js semantics. */
      val semantics: Semantics,
      /** Module kind. */
      val moduleKind: ModuleKind,
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
      val batchMode: Boolean,
      /** Standard output mode. */
      private[linker] val outputMode: OutputMode
  ) {
    private def this() = {
      this(
          semantics = Semantics.Defaults,
          moduleKind = ModuleKind.NoModule,
          checkIR = false,
          optimizer = true,
          parallel = true,
          sourceMap = true,
          relativizeSourceMapBase = None,
          closureCompilerIfAvailable = false,
          prettyPrint = false,
          batchMode = false,
          outputMode = OutputMode.Default
      )
    }

    def withSemantics(semantics: Semantics): Config =
      copy(semantics = semantics)

    def withSemantics(f: Semantics => Semantics): Config =
      copy(semantics = f(semantics))

    def withModuleKind(moduleKind: ModuleKind): Config =
      copy(moduleKind = moduleKind)

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

    private[linker] def withOutputMode(outputMode: OutputMode): Config =
      copy(outputMode = outputMode)

    override def toString(): String = {
      s"""StandardLinker.Config(
         |  semantics                  = $semantics,
         |  moduleKind                 = $moduleKind,
         |  checkIR                    = $checkIR,
         |  optimizer                  = $optimizer,
         |  parallel                   = $parallel,
         |  sourceMap                  = $sourceMap,
         |  relativizeSourceMapBase    = $relativizeSourceMapBase,
         |  closureCompilerIfAvailable = $closureCompilerIfAvailable,
         |  prettyPrint                = $prettyPrint,
         |  batchMode                  = $batchMode,
         |  outputMode                 = $outputMode,
         |)""".stripMargin
    }

    private def copy(
        semantics: Semantics = semantics,
        moduleKind: ModuleKind = moduleKind,
        checkIR: Boolean = checkIR,
        optimizer: Boolean = optimizer,
        parallel: Boolean = parallel,
        sourceMap: Boolean = sourceMap,
        relativizeSourceMapBase: Option[URI] = relativizeSourceMapBase,
        closureCompilerIfAvailable: Boolean = closureCompilerIfAvailable,
        prettyPrint: Boolean = prettyPrint,
        batchMode: Boolean = batchMode,
        outputMode: OutputMode = outputMode
    ): Config = {
      new Config(
          semantics,
          moduleKind,
          checkIR,
          optimizer,
          parallel,
          sourceMap,
          relativizeSourceMapBase,
          closureCompilerIfAvailable,
          prettyPrint,
          batchMode,
          outputMode
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
     *  - `checkIR`: `false`
     *  - `optimizer`: `true`
     *  - `parallel`: `true`
     *  - `sourceMap`: `true`
     *  - `relativizeSourceMapBase`: `None`
     *  - `closureCompilerIfAvailable`: `false`
     *  - `prettyPrint`: `false`
     *  - `batchMode`: `false`
     *
     *  The following additional options are configurable through
     *  {{{
     *  import org.scalajs.core.tools.linker.standard._
     *  }}}
     *
     *  - `outputMode`: [[org.scalajs.core.tools.linker.backend.OutputMode.Default OutputMode.Default]]
     */
    def apply(): Config = new Config()
  }

}
