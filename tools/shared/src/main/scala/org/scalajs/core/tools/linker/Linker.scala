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

  require(frontend.semantics == backend.semantics,
      "Frontend and backend must agree on semantics")
  require(frontend.esLevel == backend.esLevel,
      "Frontend and backend must agree on ESLevel")

  val semantics: Semantics = frontend.semantics
  val esLevel: ESLevel = backend.esLevel

  private[this] var _valid = true
  private[this] val _linking = new AtomicBoolean(false)

  def linkUnit(irFiles: Seq[VirtualScalaJSIRFile],
      moduleInitializers: Seq[ModuleInitializer],
      symbolRequirements: SymbolRequirement, logger: Logger): LinkingUnit =
    guard(frontend.link(irFiles, moduleInitializers, symbolRequirements, logger))

  def link(irFiles: Seq[VirtualScalaJSIRFile],
      moduleInitializers: Seq[ModuleInitializer],
      output: WritableVirtualJSFile, logger: Logger): Unit = {
    guard {
      val unit = frontend.link(irFiles, moduleInitializers,
          backend.symbolRequirements, logger)
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
      /** Additional configuration for the linker frontend. */
      val frontendConfig: LinkerFrontend.Config,
      /** Additional configuration for the linker backend. */
      val backendConfig: LinkerBackend.Config
  ) {
    @deprecated("Use config.frontendConfig.optimizer.", "0.6.17")
    val optimizer: Boolean = frontendConfig.optimizer

    @deprecated("Use config.frontendConfig.parallel.", "0.6.17")
    val parallel: Boolean = frontendConfig.parallel

    @deprecated("Use config.backendConfig.sourceMap.", "0.6.17")
    val sourceMap: Boolean = backendConfig.sourceMap

    @deprecated("Use config.backendConfig.closureCompilerIfAvailable.",
        "0.6.17")
    val closureCompilerIfAvailable = backendConfig.closureCompilerIfAvailable

    @deprecated("Use config.withBackendConfig(_.withSourceMap(sourceMap)).",
        "0.6.17")
    def withSourceMap(sourceMap: Boolean): Config =
      withBackendConfig(_.withSourceMap(sourceMap))

    @deprecated("Use config.withFrontendConfig(_.withOptimizer(optimizer)).",
        "0.6.17")
    def withOptimizer(optimizer: Boolean): Config =
      withFrontendConfig(_.withOptimizer(optimizer))

    @deprecated("Use config.withFrontendConfig(_.withParallel(parallel)).",
        "0.6.17")
    def withParallel(parallel: Boolean): Config =
      withFrontendConfig(_.withParallel(parallel))

    @deprecated(
        "Use config.withBackendConfig(_.withClosureCompilerIfAvailable(...)).",
        "0.6.17")
    def withClosureCompilerIfAvailable(closureCompilerIfAvailable: Boolean): Config =
      withBackendConfig(_.withClosureCompilerIfAvailable(closureCompilerIfAvailable))

    def withFrontendConfig(frontendConfig: LinkerFrontend.Config): Config =
      copy(frontendConfig = frontendConfig)

    def withFrontendConfig(f: LinkerFrontend.Config => LinkerFrontend.Config): Config =
      copy(frontendConfig = f(frontendConfig))

    def withBackendConfig(backendConfig: LinkerBackend.Config): Config =
      copy(backendConfig = backendConfig)

    def withBackendConfig(f: LinkerBackend.Config => LinkerBackend.Config): Config =
      copy(backendConfig = f(backendConfig))

    private def copy(
        frontendConfig: LinkerFrontend.Config = frontendConfig,
        backendConfig: LinkerBackend.Config = backendConfig): Config = {
      new Config(
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
     *  - `frontendConfig`: default frontend configuration as returned by
     *    [[org.scalajs.core.tools.linker.frontend.LinkerFrontend.Config.apply]]
     *  - `backendConfig`: default backend configuration as returned by
     *    [[org.scalajs.core.tools.linker.backend.LinkerBackend.Config.apply]]
     */
    def apply(): Config = {
      new Config(
          frontendConfig = LinkerFrontend.Config(),
          backendConfig = LinkerBackend.Config())
    }
  }
}
