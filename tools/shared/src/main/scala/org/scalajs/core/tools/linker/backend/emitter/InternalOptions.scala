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

package org.scalajs.core.tools.linker.backend.emitter

private[emitter] class InternalOptions private (
    val optimizeBracketSelects: Boolean) {

  def withOptimizeBracketSelects(optimizeBracketSelects: Boolean): InternalOptions =
    copy(optimizeBracketSelects = optimizeBracketSelects)

  private def copy(
      optimizeBracketSelects: Boolean = this.optimizeBracketSelects): InternalOptions = {
    new InternalOptions(
        optimizeBracketSelects)
  }
}

private[emitter] object InternalOptions {
  def apply(): InternalOptions = {
    new InternalOptions(
        optimizeBracketSelects = true)
  }
}
