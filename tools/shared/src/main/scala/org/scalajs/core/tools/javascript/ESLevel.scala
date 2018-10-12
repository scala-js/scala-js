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

package org.scalajs.core.tools.javascript

/** ECMAScript level the target supports */
sealed abstract class ESLevel

object ESLevel {
  case object ES5 extends ESLevel
  case object ES6 extends ESLevel
}
