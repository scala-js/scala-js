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
import org.scalajs.linker.backend.LinkerBackendImpl

object StandardLinkerBackend {
  def apply(config: StandardConfig): LinkerBackend = {
    val backendConfig = LinkerBackendImpl.Config()
      .withCommonConfig(CommonPhaseConfig.fromStandardConfig(config))
      .withJSHeader(config.jsHeader)
      .withSourceMap(config.sourceMap)
      .withOutputPatterns(config.outputPatterns)
      .withRelativizeSourceMapBase(config.relativizeSourceMapBase)
      .withMinify(config.minify)
      .withClosureCompilerIfAvailable(config.closureCompilerIfAvailable)
      .withPrettyPrint(config.prettyPrint)
      .withMaxConcurrentWrites(config.maxConcurrentWrites)
      .withExperimentalUseWebAssembly(config.experimentalUseWebAssembly)

    LinkerBackendImpl(backendConfig)
  }
}
