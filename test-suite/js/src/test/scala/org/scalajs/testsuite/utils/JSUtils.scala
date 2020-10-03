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

package org.scalajs.testsuite.utils

import scala.language.implicitConversions

import scala.scalajs.js
import js.annotation.JSExport

object JSUtils {
  /** The detected global object. */
  val globalObject: js.Dynamic = {
    import js.Dynamic.{global => g}
    if (js.typeOf(g.global) != "undefined" && (g.global.Object eq g.Object)) {
      // Node.js environment detected
      g.global
    } else {
      // In all other well-known environment, we can use the global `this`
      js.special.fileLevelThis.asInstanceOf[js.Dynamic]
    }
  }
}
