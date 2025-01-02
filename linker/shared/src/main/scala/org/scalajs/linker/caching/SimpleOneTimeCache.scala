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

/** Cache that holds a single value, computed the first time it is requested. */
final class SimpleOneTimeCache[A] extends OneTimeCache[A] {
  def getOrElseUpdate(v: => A): A =
    getOrCompute(v)
}
