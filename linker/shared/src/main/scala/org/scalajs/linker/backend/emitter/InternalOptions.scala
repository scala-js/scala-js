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

package org.scalajs.linker.backend.emitter

private[emitter] class InternalOptions private (
    val optimizeBracketSelects: Boolean,
    val trackAllGlobalRefs: Boolean
) {
  private def this() = {
    this(
        optimizeBracketSelects = true,
        trackAllGlobalRefs = false
    )
  }

  def withOptimizeBracketSelects(optimizeBracketSelects: Boolean): InternalOptions =
    copy(optimizeBracketSelects = optimizeBracketSelects)

  def withTrackAllGlobalRefs(trackAllGlobalRefs: Boolean): InternalOptions =
    copy(trackAllGlobalRefs = trackAllGlobalRefs)

  private def copy(
      optimizeBracketSelects: Boolean = this.optimizeBracketSelects,
      trackAllGlobalRefs: Boolean = this.trackAllGlobalRefs
  ): InternalOptions = {
    new InternalOptions(optimizeBracketSelects, trackAllGlobalRefs)
  }
}

private[emitter] object InternalOptions {
  def apply(): InternalOptions = new InternalOptions()
}
