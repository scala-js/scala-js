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

package scala.scalajs.js.annotation

import scala.annotation.meta._

/** Specifies the JavaScript name of an entity.
 *
 *  @see [[http://www.scala-js.org/doc/calling-javascript.html Calling JavaScript from Scala.js]]
 */
@field @getter @setter
class JSName private () extends scala.annotation.StaticAnnotation {
  def this(name: String) = this()
  def this(symbol: scala.scalajs.js.Symbol) = this()
}
