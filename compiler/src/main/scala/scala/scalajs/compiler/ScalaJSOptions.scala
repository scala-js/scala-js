/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author Tobias Schlatter
 */

package scala.scalajs.compiler

import java.net.URI

/** This trait allows to query all options to the ScalaJS plugin
 *
 *  Also see the help text in ScalaJSPlugin for information about particular
 *  options.
 */
trait ScalaJSOptions {
  /** should calls to Predef.classOf[T] be fixed in the jsinterop phase.
   *  If false, bad calls to classOf will cause an error. */
  def fixClassOf: Boolean

  /** can be used to turn off source map generation */
  def noSourceMap: Boolean

  /** URI to relativize referenced file in source maps with */
  def relSourceMap: Option[URI]

  /** URI to make referenced file absolute again (requires relSourceMap) */
  def absSourceMap: Option[URI]
}
