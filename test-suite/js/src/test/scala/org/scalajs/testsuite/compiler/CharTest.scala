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

class CharTest {
  @Test
  def `should_always_be_positive_when_coerced`(): Unit = {
    assertEquals(-3.toByte.toChar.toInt, 65533)
    assertEquals(-100.toShort.toChar.toInt, 65436)
    assertEquals(-66000.toChar.toInt, 65072)
    assertEquals(-4567L.toChar.toInt, 60969)
    assertEquals(-5.3f.toChar.toInt, 65531)
    assertEquals(-7.9.toChar.toInt, 65529)
  }

  @Test
  def `should_overflow_when_coerced`(): Unit = {
    assertEquals(347876543.toChar.toInt, 11455)
    assertEquals(34234567876543L.toChar.toInt, 57279)
  }

  @Test
  def `should_overflow_with_times`(): Unit = {
    def test(a: Char, b: Char, expected: Int): Unit =
      assertEquals(a * b, expected)

    // note: expected values are constant-folded by the compiler on the JVM
    test(Char.MaxValue, Char.MaxValue, Char.MaxValue * Char.MaxValue)
  }
}
