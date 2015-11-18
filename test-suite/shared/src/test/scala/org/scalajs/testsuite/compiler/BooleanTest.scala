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

class BooleanTest {
  @Test
  def `primitive_operations_on_booleans_should_return_correct_results`(): Unit = {
    assertFalse(false & false)
    assertFalse(false & true)
    assertFalse(true & false)
    assertTrue(true & true)

    assertFalse(false | false)
    assertTrue(true | false)
    assertTrue(false | true)
    assertTrue(true | true)

    assertFalse(false ^ false)
    assertTrue(true ^ false)
    assertTrue(false ^ true)
    assertFalse(true ^ true)
  }
}
