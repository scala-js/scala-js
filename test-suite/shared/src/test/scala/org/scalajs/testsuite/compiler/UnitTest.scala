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
