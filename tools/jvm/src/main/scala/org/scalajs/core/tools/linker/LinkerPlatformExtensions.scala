/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker

import org.scalajs.core.tools.linker.frontend.LinkerFrontend
import org.scalajs.core.tools.linker.frontend.optimizer.{ParIncOptimizer, IncOptimizer}
import org.scalajs.core.tools.linker.backend._
import org.scalajs.core.tools.linker.backend.closure.ClosureLinkerBackend

trait LinkerPlatformExtensions { this: Linker.type =>
  private[linker] def applyInternal(semantics: Semantics,
      outputMode: OutputMode, moduleKind: ModuleKind,
      config: Config): Linker = {

    val optOptimizerFactory = {
      if (!config.optimizer) None
      else if (config.parallel) Some(ParIncOptimizer.factory)
      else Some(IncOptimizer.factory)
    }

    val frontend = new LinkerFrontend(semantics, outputMode.esLevel,
        config.frontendConfig, optOptimizerFactory)

    val backend = {
      if (config.closureCompiler) {
        require(outputMode == OutputMode.ECMAScript51Isolated,
            s"Cannot use output mode $outputMode with the Closure Compiler")
        new ClosureLinkerBackend(semantics, moduleKind, config.backendConfig)
      } else {
        new BasicLinkerBackend(semantics, outputMode, moduleKind,
            config.backendConfig)
      }
    }

    new Linker(frontend, backend)
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
