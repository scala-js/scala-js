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
import org.junit.Assert.assertEquals

class RegressionTestTestScala2 {

  /** This is a Scala 2.x only test because:
   *  The extension method any2stringadd (the `+` in `x + "check"`)
   *  was deprecated in 2.13.0 and Dotty no longer has the method.
   */
  @Test def String_concatenation_with_null_issue_26(): Unit = {
    val x: Object = null
    assertEquals("nullcheck", x + "check")
  }

}
