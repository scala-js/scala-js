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

package org.scalajs.core.tools.linker

import org.scalajs.core.tools.linker.frontend.LinkerFrontend
import org.scalajs.core.tools.linker.frontend.optimizer.{ParIncOptimizer, IncOptimizer}
import org.scalajs.core.tools.linker.backend._
import org.scalajs.core.tools.linker.backend.closure.ClosureLinkerBackend

trait LinkerPlatformExtensions { this: Linker.type =>
  @deprecated("Use StandardLinker.apply() instead.", "0.6.18")
  def apply(semantics: Semantics, outputMode: OutputMode,
      moduleKind: ModuleKind, config: Config): Linker = {
    applyInternal(semantics, outputMode, moduleKind, config)
  }

  private[linker] def applyInternal(semantics: Semantics,
      outputMode: OutputMode, moduleKind: ModuleKind,
      config: Config): Linker = {

    val optOptimizerFactory = {
      if (!config.optimizer) None
      else if (config.parallel) Some(ParIncOptimizer.factory)
      else Some(IncOptimizer.factory)
    }

    val frontend = new LinkerFrontend(semantics, outputMode.esLevel,
        config.sourceMap, config.frontendConfig, optOptimizerFactory)

    val backend = {
      if (config.closureCompiler) {
        require(outputMode == OutputMode.ECMAScript51Isolated,
            s"Cannot use output mode $outputMode with the Closure Compiler")
        new ClosureLinkerBackend(semantics, moduleKind,
            config.sourceMap, config.backendConfig)
      } else {
        new BasicLinkerBackend(semantics, outputMode, moduleKind,
            config.sourceMap, config.backendConfig)
      }
    }

    new Linker(frontend, backend)
  }

  @deprecated("Use StandardLinker.apply() instead.", "0.6.13")
  def apply(
      semantics: Semantics = Semantics.Defaults,
      outputMode: OutputMode = OutputMode.Defaults,
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
    def closureCompiler: Boolean = config.closureCompilerIfAvailable

    def withClosureCompiler(closureCompiler: Boolean): Config =
      config.withClosureCompilerIfAvailable(closureCompiler)
  }
}
