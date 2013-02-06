/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend

import scalajs.JSGlobal

/** The JavaScript platform
 *  Since we try to emulate the Java platform as closely as possible (for now),
 *  we inherit from JavaPlatform.
 *
 *  @author Sébastien Doeraene
 */
trait JSPlatform extends JavaPlatform {
  val global: JSGlobal

  import global._

  override def platformPhases = List(
    flatten,
    genJSCode
  )
}
