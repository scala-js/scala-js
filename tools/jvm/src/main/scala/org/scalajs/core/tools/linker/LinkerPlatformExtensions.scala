/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker

import org.scalajs.core.tools.sem.Semantics

import org.scalajs.core.tools.linker.frontend.LinkerFrontend
import org.scalajs.core.tools.linker.frontend.optimizer.{ParIncOptimizer, IncOptimizer}
import org.scalajs.core.tools.linker.backend._
import org.scalajs.core.tools.linker.backend.closure.ClosureLinkerBackend

trait LinkerPlatformExtensions { this: Linker.type =>
  def apply(semantics: Semantics, outputMode: OutputMode,
      moduleKind: ModuleKind, config: Config): Linker = {

    val frontend = LinkerFrontend(semantics, outputMode.esLevel,
        config.frontendConfig)

    val backend = LinkerBackend(semantics, outputMode, moduleKind,
        config.backendConfig)

    new Linker(frontend, backend)
  }

  @deprecated("Use the overload with a Config object.", "0.6.13")
  def apply(
      semantics: Semantics = Semantics.Defaults,
      outputMode: OutputMode = OutputMode.Default,
      withSourceMap: Boolean = true,
      disableOptimizer: Boolean = false,
      parallel: Boolean = true,
      useClosureCompiler: Boolean = false,
      frontendConfig: LinkerFrontend.Config = LinkerFrontend.Config(),
      backendConfig: LinkerBackend.Config = LinkerBackend.Config()): Linker = {

    val config = Config()
      .withSourceMap(withSourceMap)
      .withOptimizer(!disableOptimizer)
      .withParallel(parallel)
      .withClosureCompiler(useClosureCompiler)
      .withFrontendConfig(frontendConfig)
      .withBackendConfig(backendConfig)

    apply(semantics, outputMode, ModuleKind.NoModule, config)
  }
}

object LinkerPlatformExtensions {
  import Linker.Config

  final class ConfigExt(val config: Config) extends AnyVal {
    /** Whether to actually use the Google Closure Compiler pass. */
    @deprecated("Use config.backendConfig.closureCompiler.", "0.6.17")
    def closureCompiler: Boolean = config.backendConfig.closureCompiler

    @deprecated(
        "Use config.withBackendConfig(_.withClosureCompiler(...)).",
        "0.6.17")
    def withClosureCompiler(closureCompiler: Boolean): Config =
      config.withBackendConfig(_.withClosureCompiler(closureCompiler))
  }
}
