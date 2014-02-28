/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.tools.nsc._

/** Hacks to have our source code compatible with 2.10 and 2.11.
 *  It exposes 2.11 API in a 2.10 compiler.
 *
 *  @author Sébastien Doeraene
 */
trait Compat210Component {

  val global: Global

  import global._

  // unexpandedName replaces originalName

  implicit final class SymbolCompat(self: Symbol) {
    def unexpandedName: Name = self.originalName
    def originalName: Name = sys.error("infinite loop in Compat")
  }

  // enteringPhase/exitingPhase replace beforePhase/afterPhase

  @inline final def enteringPhase[T](ph: Phase)(op: => T): T = {
    global.enteringPhase(ph)(op)
  }

  @inline final def exitingPhase[T](ph: Phase)(op: => T): T = {
    global.exitingPhase(ph)(op)
  }

  private implicit final class GlobalCompat(
      self: Compat210Component.this.global.type) {

    def enteringPhase[T](ph: Phase)(op: => T): T = self.beforePhase(ph)(op)
    def beforePhase[T](ph: Phase)(op: => T): T = sys.error("infinite loop in Compat")

    def exitingPhase[T](ph: Phase)(op: => T): T = self.afterPhase(ph)(op)
    def afterPhase[T](ph: Phase)(op: => T): T = sys.error("infinite loop in Compat")
  }

  // ErasedValueType has a different encoding

  implicit final class ErasedValueTypeCompat(self: global.ErasedValueType) {
    def valueClazz: Symbol = self.original.typeSymbol
    def original: TypeRef = sys.error("infinite loop in Compat")
  }

  // run.runDefinitions bundles methods and state related to the run
  // that were previously in definitions itself

  implicit final class RunCompat(self: Run) {
    val runDefinitions: Compat210Component.this.global.definitions.type =
      global.definitions
  }
}
