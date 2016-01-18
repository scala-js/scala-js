/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker

import org.scalajs.core.tools.sem.Semantics

import org.scalajs.core.tools.linker.frontend.LinkerFrontend
import org.scalajs.core.tools.linker.frontend.optimizer.IncOptimizer
import org.scalajs.core.tools.linker.backend._

trait LinkerPlatformExtensions { this: Linker.type =>
  def apply(
      semantics: Semantics = Semantics.Defaults,
      outputMode: OutputMode = OutputMode.Default,
      withSourceMap: Boolean = true,
      disableOptimizer: Boolean = false,
      frontendConfig: LinkerFrontend.Config = LinkerFrontend.Config(),
      backendConfig: LinkerBackend.Config = LinkerBackend.Config()): Linker = {
    val optOptimizerFactory =
      if (disableOptimizer) None
      else Some(IncOptimizer.factory)

    val frontend = new LinkerFrontend(semantics, outputMode.esLevel,
        withSourceMap, frontendConfig, optOptimizerFactory)
    val backend = new BasicLinkerBackend(semantics, outputMode, withSourceMap,
        backendConfig)
    new Linker(frontend, backend)
  }
}
