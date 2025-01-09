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

final class SimpleInputEqualityCache[I, A] extends InputEqualityCache[I, A] {
  def getOrElseUpdate(input: I, v: => A): A =
    getOrCompute(input, v)
}
