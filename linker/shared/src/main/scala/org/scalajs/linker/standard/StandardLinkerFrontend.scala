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

package org.scalajs.linker.standard

import org.scalajs.linker.interface.StandardConfig
import org.scalajs.linker.frontend.LinkerFrontendImpl

object StandardLinkerFrontend {
  def apply(config: StandardConfig): LinkerFrontend = {
    val frontendConfig = LinkerFrontendImpl.Config()
      .withCommonConfig(CommonPhaseConfig.fromStandardConfig(config))
      .withModuleSplitStyle(config.moduleSplitStyle)
      .withCheckIR(config.checkIR)
      .withOptimizer(config.optimizer)

    LinkerFrontendImpl(frontendConfig)
  }
}
