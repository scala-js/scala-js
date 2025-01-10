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

package org.scalajs.linker.checker

final class IRFeature(val displayName: String) {
  override def toString(): String = displayName
}

object IRFeature {
  /** IR coming after an initial linking phase. */
  val Linked = new IRFeature("linked")

  /** IR that must be desugared away. */
  val NeedsDesugaring = new IRFeature("not-desugared")

  /** IR that is only the result of desugaring. */
  val Desugared = new IRFeature("desugared")

  /** IR that is only the result of optimizations. */
  val Optimized = new IRFeature("optimized")
}
