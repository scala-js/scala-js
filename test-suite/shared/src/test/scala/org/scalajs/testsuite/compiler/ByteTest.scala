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

class ByteTest {

  @Test
  def `should_always_be_in_their_range`(): Unit = {
    def test(x: Int, y: Byte): Unit =
      assertEquals(y, x.toByte)

    test(0, 0)
    test(127, 127)
    test(128, -128)
    test(-128, -128)
    test(-500, 12)
    test(-90000, 112)
    test(123456789, 21)
    test(-40000, -64)
    test(65536, 0)
    test(32768, 0)

    def testC(x: Char, y: Byte): Unit =
      assertEquals(y, x.toByte)

    testC(-1.toChar, -1)
    testC(200.toChar, -56)
  }
}
