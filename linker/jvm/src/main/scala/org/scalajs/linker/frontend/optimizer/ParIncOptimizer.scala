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

package org.scalajs.linker.frontend.optimizer

import org.scalajs.linker.standard.CommonPhaseConfig

object ParIncOptimizer {
  def apply(config: CommonPhaseConfig): GenIncOptimizer =
    new GenIncOptimizer(config, ParCollOps)
}
