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

package org.scalajs.linker

import org.scalajs.linker.interface._
import org.scalajs.linker.standard._

object StandardImpl {
  def irFileCache(): IRFileCache =
    irFileCache(IRFileCacheConfig())

  def irFileCache(config: IRFileCacheConfig): IRFileCache =
    new StandardIRFileCache(config)

  def linker(config: StandardConfig): Linker = {
    StandardLinkerImpl(StandardLinkerFrontend(config), StandardLinkerBackend(config))
  }

  def clearableLinker(config: StandardConfig): ClearableLinker =
    ClearableLinker(() => linker(config), config.batchMode)
}
