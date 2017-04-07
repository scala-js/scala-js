/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author Tobias Schlatter
 */

package org.scalajs.core.compiler

import java.net.URI

/** This trait allows to query all options to the ScalaJS plugin
 *
 *  Also see the help text in ScalaJSPlugin for information about particular
 *  options.
 */
trait ScalaJSOptions {
  import ScalaJSOptions.URIMap

  /** should calls to Predef.classOf[T] be fixed in the jsinterop phase.
   *  If false, bad calls to classOf will cause an error. */
  def fixClassOf: Boolean

  /** which source locations in source maps should be relativized (or where
   *  should they be mapped to)? */
  def sourceURIMaps: List[URIMap]

}

object ScalaJSOptions {
  case class URIMap(from: URI, to: Option[URI])
}
