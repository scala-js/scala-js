/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.core.tools.linker.frontend

import org.scalajs.core.tools.linker.frontend.optimizer._

private[frontend] object LinkerFrontendPlatform {
  import LinkerFrontend.Config

  def createOptimizer(config: Config): Option[GenIncOptimizer] = {
    if (!config.optimizer)
      None
    else
      Some(new IncOptimizer(config.commonConfig))
  }
}
