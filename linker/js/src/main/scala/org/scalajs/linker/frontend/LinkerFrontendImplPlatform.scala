/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.linker.frontend

import org.scalajs.linker.frontend.optimizer._

private[frontend] object LinkerFrontendImplPlatform {
  import LinkerFrontendImpl.Config

  def createOptimizer(config: Config): Option[GenIncOptimizer] = {
    if (!config.optimizer)
      None
    else
      Some(new IncOptimizer(config.commonConfig))
  }
}
