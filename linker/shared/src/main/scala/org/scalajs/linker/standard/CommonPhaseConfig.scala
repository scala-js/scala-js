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

import org.scalajs.linker.interface._

/** Common configuration given to all phases of the linker. */
final class CommonPhaseConfig private (
    /** Core specification. */
    val coreSpec: CoreSpec,
    /** Whether we are compiling to WebAssembly. */
    val targetIsWebAssembly: Boolean,
    /** Apply Scala.js-specific minification of the produced .js files. */
    val minify: Boolean,
    /** Whether things that can be parallelized should be parallelized.
     *  On the JavaScript platform, this setting is typically ignored.
     */
    val parallel: Boolean,
    /** Whether the linker runs in batch mode.
     *
     *  In batch mode, the linker phase can throw away intermediate state that
     *  is otherwise maintained for incremental runs.
     *
     *  This setting is only a hint. A linker phase may ignore it. This applies
     *  in both directions: a phase not supporting incrementality can ignore
     *  `batchMode = false`, and a contrario, a phase mainly designed for
     *  incremental runs may ignore `batchMode = true`.
     */
    val batchMode: Boolean
) {
  private def this() = {
    this(
        coreSpec = CoreSpec.Defaults,
        targetIsWebAssembly = false,
        minify = false,
        parallel = true,
        batchMode = false)
  }
}

private[linker] object CommonPhaseConfig {
  private[linker] def apply(): CommonPhaseConfig = new CommonPhaseConfig()

  private[linker] def fromStandardConfig(config: StandardConfig): CommonPhaseConfig = {
    val coreSpec = CoreSpec(config.semantics, config.moduleKind, config.esFeatures)
    new CommonPhaseConfig(
      coreSpec,
      config.experimentalUseWebAssembly,
      config.minify,
      config.parallel,
      config.batchMode
    )
  }
}
