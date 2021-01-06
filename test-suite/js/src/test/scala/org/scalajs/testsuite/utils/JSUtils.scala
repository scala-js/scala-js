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

import scala.scalajs.js
import js.annotation.JSExport

object JSUtils {
  /** The detected global object. */
  val globalObject: js.Dynamic = {
    import js.Dynamic.{global => g}

    def check(o: js.Dynamic) = o.Object eq g.Object

    if (js.typeOf(g.global) != "undefined" && check(g.global)) {
      // Node.js environment detected
      g.global
    } else if (js.typeOf(g.window) != "undefined" && check(g.window)) {
      // DOM environment detected
      g.window
    } else {
      assert(Platform.isNoModule, "Failed to detect global object in module")
      js.special.fileLevelThis.asInstanceOf[js.Dynamic]
    }
  }
}
