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

package org.scalajs.linker.backend

import org.scalajs.linker.backend.closure.ClosureLinkerBackend

private[backend] object LinkerBackendImplPlatform {
  import LinkerBackendImpl.Config

  def createJSLinkerBackend(config: Config): LinkerBackendImpl = {
    if (config.closureCompiler)
      new ClosureLinkerBackend(config)
    else
      new BasicLinkerBackend(config)
  }
}
