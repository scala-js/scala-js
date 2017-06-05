/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.core.tools.linker.frontend

import org.scalajs.core.tools.javascript.ESLevel
import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.linker.frontend.optimizer.{ParIncOptimizer, IncOptimizer}

trait LinkerFrontendPlatformExtensions { this: LinkerFrontend.type =>
  def apply(semantics: Semantics, esLevel: ESLevel,
      config: Config): LinkerFrontend = {

    val optOptimizerFactory = {
      if (!config.optimizer) None
      else if (config.parallel) Some(ParIncOptimizer.factory)
      else Some(IncOptimizer.factory)
    }

    new LinkerFrontend(semantics, esLevel, config, optOptimizerFactory)
  }
}
