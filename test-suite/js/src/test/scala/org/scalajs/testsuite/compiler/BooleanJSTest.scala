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
