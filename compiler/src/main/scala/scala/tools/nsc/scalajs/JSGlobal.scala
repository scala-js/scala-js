/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package scalajs

import backend.js._

import util.returning

/** Additions to Global for the JavaScript backend
 *
 *  @author Sébastien Doeraene
 */
trait JSGlobal extends Global {
  /** Addons for JavaScript platform */
  object jsAddons extends {
    val global: JSGlobal.this.type = JSGlobal.this
  } with JSGlobalAddons

  // phaseName = "jscode"
  object genJSCode extends {
    val global: JSGlobal.this.type = JSGlobal.this
    val jsAddons: JSGlobal.this.jsAddons.type = JSGlobal.this.jsAddons
    val runsAfter = List("mixin")
    override val runsBefore = List("cleanup", "terminal")
    override val runsRightAfter = None
  } with GenJSCode

  override protected def computePlatformPhases() {
    super.computePlatformPhases()

    phasesSet += genJSCode
  }
}
