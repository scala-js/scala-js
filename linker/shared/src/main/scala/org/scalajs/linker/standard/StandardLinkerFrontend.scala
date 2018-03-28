/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js linker            **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2018, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.linker.standard

import org.scalajs.linker._
import org.scalajs.linker.frontend.LinkerFrontendImpl

object StandardLinkerFrontend {
  def apply(config: StandardLinker.Config): LinkerFrontend = {
    val frontendConfig = LinkerFrontendImpl.Config()
      .withCommonConfig(config.commonPhaseConfig)
      .withCheckIR(config.checkIR)
      .withOptimizer(config.optimizer)

    LinkerFrontendImpl(frontendConfig)
  }
}
