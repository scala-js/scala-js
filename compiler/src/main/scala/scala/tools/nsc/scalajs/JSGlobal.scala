/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.tools.nsc
package scalajs

import backend.js._

import util.returning

/** A specialized Global that enables the JavaScript backend by default
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
