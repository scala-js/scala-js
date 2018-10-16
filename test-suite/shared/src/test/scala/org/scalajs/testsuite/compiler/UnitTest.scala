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

class UnitTest {
  @Test def `should_have_hashCode`(): Unit = {
    assertEquals(0, ().hashCode())
    assertEquals(0, ((): Any).hashCode())
    assertEquals(0, ().##)
  }

  @Test def `should_equal_itself`(): Unit = {
    assertTrue(().equals(()))
    assertTrue(((): Any).equals((): Any))
  }

  @Test def `should_not_equal_other_values`(): Unit = {
    def testAgainst(v: Any): Unit = {
      assertFalse(().equals(v))
      assertFalse(((): Any).equals(v))
    }

    testAgainst(0)
    testAgainst(1)
    testAgainst(null)
    testAgainst(false)
    testAgainst("")
  }
}
