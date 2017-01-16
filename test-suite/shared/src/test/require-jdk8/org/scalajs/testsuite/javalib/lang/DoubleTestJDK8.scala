/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scalajs.testsuite.javalib.lang

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform.executingInJVM

import java.lang.{Double => JDouble}

class DoubleTestJDK8 {

  @Test def isFinite(): Unit = {
    assertFalse(JDouble.isFinite(Double.PositiveInfinity))
    assertFalse(JDouble.isFinite(Double.NegativeInfinity))
    assertFalse(JDouble.isFinite(Double.NaN))
    assertFalse(JDouble.isFinite(1d/0))
    assertFalse(JDouble.isFinite(-1d/0))

    assertTrue(JDouble.isFinite(0d))
    assertTrue(JDouble.isFinite(1d))
    assertTrue(JDouble.isFinite(123456d))
    assertTrue(JDouble.isFinite(Double.MinValue))
    assertTrue(JDouble.isFinite(Double.MaxValue))
    assertTrue(JDouble.isFinite(Double.MinPositiveValue))
  }

  @Test def staticHashCodeTest(): Unit = {
    assumeFalse("Hash codes for doubles are different in JS than on the JVM",
        executingInJVM)

    def test(x: Double, expected: Int): Unit =
      assertEquals(expected, JDouble.hashCode(x))

    test(0.0, 0)
    test(-0.0, -2147483648)
    test(1234.0, 1234)
    test(1.5, 1073217536)
    test(Math.PI, 340593891)
    test(-54.0, -54)

    test(Double.MinPositiveValue, 1)
    test(Double.MinValue, 1048576)
    test(Double.MaxValue, -2146435072)

    test(Double.NaN, 2146959360)
    test(Double.PositiveInfinity, 2146435072)
    test(Double.NegativeInfinity, -1048576)
  }

  // The following tests are only to make sure that things link

  @Test def sum(): Unit = {
    assertEquals(12d, JDouble.sum(5d, 7d), 0d)
  }

  @Test def max(): Unit = {
    assertEquals(7d, JDouble.max(5d, 7d), 0d)
  }

  @Test def min(): Unit = {
    assertEquals(5d, JDouble.min(5d, 7d), 0d)
  }

}
