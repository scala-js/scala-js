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

  /** Should we suppress deprecations of exports coming from 0.6.15? */
  def suppressExportDeprecations: Boolean

  /** Should we suppress deprecations related to missing `@JSGlobal`? */
  def suppressMissingJSGlobalDeprecations: Boolean

  /** which source locations in source maps should be relativized (or where
   *  should they be mapped to)? */
  def sourceURIMaps: List[URIMap]

  /** Switch the default for JS types from `@js.native` to `@ScalaJSDefined`.
   *  This is intended as a transition option between 0.6.x and 1.x.
   */
  def sjsDefinedByDefault: Boolean

}

object ScalaJSOptions {
  case class URIMap(from: URI, to: Option[URI])
}
