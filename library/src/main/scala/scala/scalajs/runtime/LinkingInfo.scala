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

package scala.scalajs.runtime

import scala.scalajs.js

/** Information about link-time configuration of Scala.js. */
sealed trait LinkingInfo extends js.Object {
  /** Whether we are assuming ECMAScript 6 support or not. */
  val assumingES6: Boolean

  /** Whether we are linking in production mode. */
  val productionMode: Boolean

  /** Version of the linker */
  val linkerVersion: String

  /** The value of the global JavaScript `this`. */
  val globalThis: Any
}
