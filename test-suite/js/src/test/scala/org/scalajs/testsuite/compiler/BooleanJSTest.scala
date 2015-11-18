/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import org.junit.Test
import org.junit.Assert._

import scala.scalajs.js

class BooleanJSTest {
  @Test
  def `primitive_operations_on_booleans_should_return_boolean`(): Unit = {
    // FIXME: these tests are completely useless:
    // they're constant-folded by scalac. We're not at all testing those
    // operations are the IR level, nor, a fortiori, at the JS level
    assertEquals(js.typeOf(true & false), "boolean")
    assertEquals(js.typeOf(true | false), "boolean")
    assertEquals(js.typeOf(true ^ false), "boolean")
  }
}
