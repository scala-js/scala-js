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

import org.scalajs.core.tools.linker.frontend.LinkerFrontend
import org.scalajs.core.tools.linker.backend.{LinkerBackend, OutputMode}

object StandardLinker {
  import StandardLinkerPlatformExtensions._

  def apply(config: Config): Linker = {
    val frontendConfig = LinkerFrontend.Config()
      .withBypassLinkingErrorsInternal(config.bypassLinkingErrors)
      .withCheckIR(config.checkIR)

    val backendConfig = LinkerBackend.Config()
      .withRelativizeSourceMapBase(config.relativizeSourceMapBase)
      .withCustomOutputWrapperInternal(config.customOutputWrapper)
      .withPrettyPrint(config.prettyPrint)

    val oldAPIConfig = Linker.Config()
      .withSourceMap(config.sourceMap)
      .withOptimizer(config.optimizer)
      .withParallel(config.parallel)
      .withClosureCompilerIfAvailable(config.closureCompilerIfAvailable)
      .withFrontendConfig(frontendConfig)
      .withBackendConfig(backendConfig)

    Linker.applyInternal(config.semantics, config.outputMode, config.moduleKind,
        oldAPIConfig)
  }

  implicit def configExt(config: Config): ConfigExt =
    new ConfigExt(config)

  /** Configuration of a standard linker. */
  final class Config private (
      /** Scala.js semantics. */
      val semantics: Semantics,
      /** Module kind. */
      val moduleKind: ModuleKind,
      /** Whether to only warn if the linker has errors. */
      val bypassLinkingErrors: Boolean,
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
      /** Custom js code that wraps the output */
      val customOutputWrapper: (String, String),
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
          bypassLinkingErrors = false,
          checkIR = false,
          optimizer = true,
          parallel = true,
          sourceMap = true,
          relativizeSourceMapBase = None,
          customOutputWrapper = ("", ""),
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

    @deprecated(
        "Bypassing linking errors will not be possible in the next major version.",
        "0.6.6")
    def withBypassLinkingErrors(bypassLinkingErrors: Boolean): Config =
      copy(bypassLinkingErrors = bypassLinkingErrors)

    // Non-deprecated version to call from the sbt plugin
    private[scalajs] def withBypassLinkingErrorsInternal(
        bypassLinkingErrors: Boolean): Config = {
      copy(bypassLinkingErrors = bypassLinkingErrors)
    }

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

    def withBatchMode(batchMode: Boolean): Config =
      copy(batchMode = batchMode)

    private[linker] def withOutputMode(outputMode: OutputMode): Config =
      copy(outputMode = outputMode)

    override def toString(): String = {
      s"""StandardLinker.Config(
         |  semantics                  = $semantics,
         |  moduleKind                 = $moduleKind,
         |  bypassLinkingErrors        = $bypassLinkingErrors,
         |  checkIR                    = $checkIR,
         |  optimizer                  = $optimizer,
         |  parallel                   = $parallel,
         |  sourceMap                  = $sourceMap,
         |  relativizeSourceMapBase    = $relativizeSourceMapBase,
         |  customOutputWrapper        = $customOutputWrapper,
         |  closureCompilerIfAvailable = $closureCompilerIfAvailable,
         |  prettyPrint                = $prettyPrint,
         |  batchMode                  = $batchMode,
         |  outputMode                 = $outputMode,
         |)""".stripMargin
    }

    private def copy(
        semantics: Semantics = semantics,
        moduleKind: ModuleKind = moduleKind,
        bypassLinkingErrors: Boolean = bypassLinkingErrors,
        checkIR: Boolean = checkIR,
        optimizer: Boolean = optimizer,
        parallel: Boolean = parallel,
        sourceMap: Boolean = sourceMap,
        relativizeSourceMapBase: Option[URI] = relativizeSourceMapBase,
        customOutputWrapper: (String, String) = customOutputWrapper,
        closureCompilerIfAvailable: Boolean = closureCompilerIfAvailable,
        prettyPrint: Boolean = prettyPrint,
        batchMode: Boolean = batchMode,
        outputMode: OutputMode = outputMode
    ): Config = {
      new Config(
          semantics,
          moduleKind,
          bypassLinkingErrors,
          checkIR,
          optimizer,
          parallel,
          sourceMap,
          relativizeSourceMapBase,
          customOutputWrapper,
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
     *  - `semantics`: [[org.scalajs.core.tools.sem.Semantics.Defaults Semantics.Defaults]]
     *  - `moduleKind`: [[ModuleKind.NoModule]]
     *  - `bypassLinkingErrors`: `false` (deprecated)
     *  - `checkIR`: `false`
     *  - `optimizer`: `true`
     *  - `parallel`: `true`
     *  - `sourceMap`: `true`
     *  - `relativizeSourceMapBase`: `None`
     *  - `customOutputWrapper`: `("", "")` (deprecated)
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
