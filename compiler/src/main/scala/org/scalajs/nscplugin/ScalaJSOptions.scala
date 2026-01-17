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

package org.scalajs.nscplugin

import java.net.URI

/** This trait allows to query all options to the ScalaJS plugin
 *
 *  Also see the help text in ScalaJSPlugin for information about particular
 *  options.
 */
trait ScalaJSOptions {
  import ScalaJSOptions.URIMap

  /** should calls to Predef.classOf[T] be fixed in the jsinterop phase.
   *  If false, bad calls to classOf will cause an error.
   */
  def fixClassOf: Boolean

  /** Should static forwarders be emitted for non-top-level objects.
   *
   *  Scala/JVM does not do that. Since Scala.js 1.2.0, we do not do it by
   *  default either, but this option can be used to opt in. This is necessary
   *  for implementations of JDK classes.
   */
  def genStaticForwardersForNonTopLevelObjects: Boolean

  /** which source locations in source maps should be relativized (or where
   *  should they be mapped to)?
   */
  def sourceURIMaps: List[URIMap]

  /** Whether to warn if the global execution context is used.
   *
   *  See the warning itself or #4129 for context.
   */
  def warnGlobalExecutionContext: Boolean
}

object ScalaJSOptions {
  case class URIMap(from: URI, to: Option[URI])
}
