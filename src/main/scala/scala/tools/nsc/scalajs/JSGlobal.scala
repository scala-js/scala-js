/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package scalajs

import backend.JSPlatform
import backend.js._

import util.returning

/** Additions to Global for the JavaScript backend
 *
 *  @author Sébastien Doeraene
 */
trait JSGlobal extends Global
                  with JSTrees
                  with JSPrinters {
  /** Platform */
  override lazy val platform: ThisPlatform =
    new { val global: JSGlobal.this.type = JSGlobal.this } with JSPlatform

  // phaseName = "jscode"
  object genJSCode extends {
    val global: JSGlobal.this.type = JSGlobal.this
    val runsAfter = List("dce")
    val runsRightAfter = None
  } with GenJSCode

  // phaseName = "terminal" (unfortunately we need to copy it from Global)
  object jsTerminal extends {
    val global: JSGlobal.this.type = JSGlobal.this
    val phaseName = "terminal"
    val runsAfter = List("jvm", "msil", "jscode") // added "jscode"
    val runsRightAfter = None
  } with SubComponent {
    private var cache: Option[GlobalPhase] = None
    def reset(): Unit = cache = None

    def newPhase(prev: Phase): GlobalPhase =
      cache getOrElse returning(new TerminalPhase(prev))(x => cache = Some(x))

    class TerminalPhase(prev: Phase) extends GlobalPhase(prev) {
      def name = "terminal"
      def apply(unit: CompilationUnit) {}
    }
  }
}
