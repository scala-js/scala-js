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
      js.special.globalThis.asInstanceOf[js.Dynamic]
    }
  }

  /* We use java.lang.Character explicitly, because this class is used by
   * tests that check that Chars are actually boxed by the compiler.
   * If we rely on the compiler doing the job in here, we might have false
   * positives just because the value was never boxed, and never unboxed.
   */

  @JSExport
  def isChar(c: Any): Boolean = c.isInstanceOf[java.lang.Character]

  @JSExport
  def stringToChar(s: String): java.lang.Character = {
    assert(s.length == 1, "makeChar() requires a string of length 1")
    new java.lang.Character(s.charAt(0))
  }

  @JSExport
  def charToString(c: Any): String =
    c.asInstanceOf[java.lang.Character].toString()

  implicit def asJSAny(jsUtils: JSUtils.type): js.Any =
    jsUtils.asInstanceOf[js.Any]
}
