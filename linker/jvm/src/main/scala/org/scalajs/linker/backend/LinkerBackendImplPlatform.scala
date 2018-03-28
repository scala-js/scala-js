/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.linker.backend

import org.scalajs.linker.backend.closure.ClosureLinkerBackend

private[backend] object LinkerBackendImplPlatform {
  import LinkerBackendImpl.Config

  def createLinkerBackend(config: Config): LinkerBackendImpl = {
    if (config.closureCompiler)
      new ClosureLinkerBackend(config)
    else
      new BasicLinkerBackend(config)
  }
}
