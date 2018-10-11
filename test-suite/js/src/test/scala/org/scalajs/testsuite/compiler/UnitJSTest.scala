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

class UnitJSTest {
  @Test def `should_have_toString()`(): Unit = {
    assertEquals(().toString(),  "undefined")
    assertEquals(((): Any).toString(),  "undefined")
  }
}
