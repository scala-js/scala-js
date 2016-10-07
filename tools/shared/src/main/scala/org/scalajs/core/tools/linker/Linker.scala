/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker

import scala.language.implicitConversions

import java.util.concurrent.atomic.AtomicBoolean

import org.scalajs.core.tools.logging.Logger
import org.scalajs.core.tools.io._

import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.javascript.ESLevel
import org.scalajs.core.tools.linker.analyzer.SymbolRequirement
import org.scalajs.core.tools.linker.frontend.LinkerFrontend
import org.scalajs.core.tools.linker.frontend.optimizer.IncOptimizer
import org.scalajs.core.tools.linker.backend.{LinkerBackend, BasicLinkerBackend}

/** The Scala.js linker */
final class Linker(frontend: LinkerFrontend, backend: LinkerBackend)
    extends GenLinker {

  require(!backend.withSourceMap || frontend.withSourceMap,
      "Frontend must have source maps enabled if backend has them enabled")
  require(frontend.semantics == backend.semantics,
      "Frontend and backend must agree on semantics")
  require(frontend.esLevel == backend.esLevel,
      "Frontend and backend must agree on ESLevel")

  val semantics: Semantics = frontend.semantics
  val esLevel: ESLevel = backend.esLevel

  private[this] var _valid = true
  private[this] val _linking = new AtomicBoolean(false)

  def linkUnit(irFiles: Seq[VirtualScalaJSIRFile],
      symbolRequirements: SymbolRequirement, logger: Logger): LinkingUnit =
    guard(frontend.link(irFiles, symbolRequirements, logger))

  def link(irFiles: Seq[VirtualScalaJSIRFile],
      output: WritableVirtualJSFile, logger: Logger): Unit = {
    guard {
      val unit = frontend.link(irFiles, backend.symbolRequirements, logger)
      backend.emit(unit, output, logger)
    }
  }

  @inline
  private[this] def guard[T](body: => T): T = {
    if (!_linking.compareAndSet(false, true)) {
      throw new IllegalStateException("Linker used concurrently")
    }

    try {
      if (!_valid) {
        throw new IllegalStateException(
          "Linker is invalid due to a previous exception in a component")
      }

      body
    } catch {
      case t: Throwable =>
        _valid = false
        throw t
    } finally {
      _linking.set(false)
    }
  }
}

object Linker extends LinkerPlatformExtensions {
  /** Configuration to be passed to the `apply()` method. */
  final class Config private (
      /** Whether to generate source maps. */
      val sourceMap: Boolean,
      /** Whether to use the Scala.js optimizer. */
      val optimizer: Boolean,
      /** Whether things that can be parallelized should be parallelized.
       *  On the JavaScript platform, this does not have any effect.
       */
      val parallel: Boolean,
      /** Whether to use the Google Closure Compiler pass, if it is available.
       *  On the JavaScript platform, this does not have any effect.
       */
      val closureCompilerIfAvailable: Boolean,
      /** Additional configuration for the linker frontend. */
      val frontendConfig: LinkerFrontend.Config,
      /** Additional configuration for the linker backend. */
      val backendConfig: LinkerBackend.Config
  ) {
    def withSourceMap(sourceMap: Boolean): Config =
      copy(sourceMap = sourceMap)

    def withOptimizer(optimizer: Boolean): Config =
      copy(optimizer = optimizer)

    def withParallel(parallel: Boolean): Config =
      copy(parallel = parallel)

    def withClosureCompilerIfAvailable(closureCompilerIfAvailable: Boolean): Config =
      copy(closureCompilerIfAvailable = closureCompilerIfAvailable)

    def withFrontendConfig(frontendConfig: LinkerFrontend.Config): Config =
      copy(frontendConfig = frontendConfig)

    def withBackendConfig(backendConfig: LinkerBackend.Config): Config =
      copy(backendConfig = backendConfig)

    private def copy(
        sourceMap: Boolean = sourceMap,
        optimizer: Boolean = optimizer,
        parallel: Boolean = parallel,
        closureCompilerIfAvailable: Boolean = closureCompilerIfAvailable,
        frontendConfig: LinkerFrontend.Config = frontendConfig,
        backendConfig: LinkerBackend.Config = backendConfig): Config = {
      new Config(
          sourceMap = sourceMap,
          optimizer = optimizer,
          parallel = parallel,
          closureCompilerIfAvailable = closureCompilerIfAvailable,
          frontendConfig = frontendConfig,
          backendConfig = backendConfig)
    }
  }

  object Config {
    import LinkerPlatformExtensions._

    implicit def toPlatformExtensions(config: Config): ConfigExt =
      new ConfigExt(config)

    /** Default configuration.
     *
     *  - `sourceMap`: true
     *  - `optimizer`: true
     *  - `parallel`: true
     *  - `closureCompilerIfAvailable`: false
     *  - `frontendConfig`: default frontend configuration as returned by
     *    [[org.scalajs.core.tools.linker.frontend.LinkerFrontend.Config.apply]]
     *  - `backendConfig`: default backend configuration as returned by
     *    [[org.scalajs.core.tools.linker.backend.LinkerBackend.Config.apply]]
     */
    def apply(): Config = {
      new Config(
          sourceMap = true,
          optimizer = true,
          parallel = true,
          closureCompilerIfAvailable = false,
          frontendConfig = LinkerFrontend.Config(),
          backendConfig = LinkerBackend.Config())
    }
  }
}
