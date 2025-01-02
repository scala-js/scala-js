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

package org.scalajs.linker.caching

import org.scalajs.ir.Version

/** A cache for a single value that gets invalidated based on a `Version`. */
class SimpleVersionedCache[T] extends VersionedCache[T] {
  final def getOrElseUpdate(version: Version, computeValue: => T): T =
    getOrCompute(version, computeValue)

  final def getOrElseUpdateWithChanged(version: Version, computeValue: => T): (T, Boolean) =
    getOrComputeWithChanged(version, computeValue)
}
