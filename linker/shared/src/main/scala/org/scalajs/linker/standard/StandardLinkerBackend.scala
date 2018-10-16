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

import org.scalajs.linker._
import org.scalajs.linker.backend.LinkerBackendImpl

object StandardLinkerBackend {
  def apply(config: StandardLinker.Config): LinkerBackend = {
    val backendConfig = LinkerBackendImpl.Config()
      .withCommonConfig(config.commonPhaseConfig)
      .withSourceMap(config.sourceMap)
      .withRelativizeSourceMapBase(config.relativizeSourceMapBase)
      .withClosureCompilerIfAvailable(config.closureCompilerIfAvailable)
      .withPrettyPrint(config.prettyPrint)

    LinkerBackendImpl(backendConfig)
  }
}
