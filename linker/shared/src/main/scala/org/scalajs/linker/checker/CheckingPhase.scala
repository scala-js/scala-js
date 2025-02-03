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

/** A phase *after which* we are checking IR.
 *
 *  When checking IR (with `ClassDefChecker` or `IRChecker`), different nodes
 *  and transients are allowed between different phases. The `CheckingPhase`
 *  records the *previous* phase to run before the check. We are therefore
 *  checking that the IR is a valid *output* of the target phase.
 */
sealed abstract class CheckingPhase

object CheckingPhase {
  case object Compiler extends CheckingPhase
  case object BaseLinker extends CheckingPhase
  case object Desugarer extends CheckingPhase
  case object Optimizer extends CheckingPhase
}
