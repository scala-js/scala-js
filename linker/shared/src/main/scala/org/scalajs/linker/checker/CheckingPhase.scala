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

/** A phase *before which* we are checking IR for.
 *
 *  When checking IR (with `ClassDefChecker` or `IRChecker`), different nodes
 *  and transients are allowed between different phases. The `CheckingPhase`
 *  records the *next* phase to run after the check. We are therefore checking
 *  that the IR is a valid *input* to the target phase.
 */
abstract class CheckingPhase private[checker] () {
  def accept(feature: IRFeature): Boolean
}

object CheckingPhase {
  object BaseLinker extends CheckingPhase {
    def accept(feature: IRFeature): Boolean = feature match {
      case IRFeature.NeedsDesugaring =>
        true
      case _ =>
        false
    }
  }

  object Desugarer extends CheckingPhase {
    def accept(feature: IRFeature): Boolean = feature match {
      case IRFeature.Linked | IRFeature.NeedsDesugaring =>
        true
      case _ =>
        false
    }
  }

  final class Emitter(afterOptimizer: Boolean) extends CheckingPhase {
    def accept(feature: IRFeature): Boolean = feature match {
      case IRFeature.Linked | IRFeature.Desugared =>
        true
      case IRFeature.Optimized =>
        afterOptimizer
      case _ =>
        false
    }
  }

  object Emitter {
    def apply(afterOptimizer: Boolean): Emitter =
      new Emitter(afterOptimizer)
  }

  object Optimizer extends CheckingPhase {
    // The optimizer must accept everythin an emitter without optimizer accepts
    private val delegate = new Emitter(afterOptimizer = false)

    def accept(feature: IRFeature): Boolean =
      delegate.accept(feature)
  }
}
