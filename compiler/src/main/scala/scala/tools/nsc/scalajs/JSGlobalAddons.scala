/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.tools.nsc
package scalajs

import backend.js._

/** Additions to Global meaningful for the JavaScript backend
 *
 *  @author Sébastien Doeraene
 */
trait JSGlobalAddons extends JSTrees with JSPrinters with JSDefinitions {
  val global: Global

  /** JavaScript primitives, used in jscode */
  object jsPrimitives extends JSPrimitives {
    val global: JSGlobalAddons.this.global.type = JSGlobalAddons.this.global
    val jsAddons: ThisJSGlobalAddons =
      JSGlobalAddons.this.asInstanceOf[ThisJSGlobalAddons]
  }
}
