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

class ShortTest {
  @Test def `should_always_be_in_their_range`(): Unit = {
    def test(x: Int, y: Short): Unit =
      assertEquals(y, x.toShort)

    test(0, 0)
    test(-500, -500)
    test(-90000, -24464)
    test(123456789, -13035)
    test(-40000, 25536)
    test(65536, 0)
    test(32768, -32768)

    def testC(x: Char, y: Short): Unit =
      assertEquals(x.toShort, y)

    testC(-1.toChar, -1)
    testC(200.toChar, 200)
    testC(60000.toChar, -5536)
  }
}
