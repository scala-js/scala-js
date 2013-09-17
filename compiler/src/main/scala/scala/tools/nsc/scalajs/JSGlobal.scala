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
                  with JSPrinters
                  with JSDefinitions {
  /** Platform */
  override lazy val platform: ThisPlatform =
    new { val global: JSGlobal.this.type = JSGlobal.this } with JSPlatform

  /** JavaScript primitives, used in jscode */
  object jsPrimitives extends {
    val global: JSGlobal.this.type = JSGlobal.this
  } with JSPrimitives

  // phaseName = "jscode"
  object genJSCode extends {
    val global: JSGlobal.this.type = JSGlobal.this
    val runsAfter = List("mixin")
    override val runsBefore = List("terminal")
    val runsRightAfter = None
  } with GenJSCode

  override protected def computeInternalPhases() {
    super.computeInternalPhases()

    // Remove some phases not used by the JS backend
    phasesSet --= Seq(cleanup, genicode, inliner, inlineExceptionHandlers,
        closureElimination, deadCode)
  }
}
