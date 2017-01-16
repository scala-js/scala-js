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

import java.lang.{Float => JFloat}

class FloatTestJDK8 {

  @Test def isFinite(): Unit = {
    assertFalse(JFloat.isFinite(Float.PositiveInfinity))
    assertFalse(JFloat.isFinite(Float.NegativeInfinity))
    assertFalse(JFloat.isFinite(Float.NaN))
    assertFalse(JFloat.isFinite(1f/0))
    assertFalse(JFloat.isFinite(-1f/0))

    assertTrue(JFloat.isFinite(0f))
    assertTrue(JFloat.isFinite(1f))
    assertTrue(JFloat.isFinite(123456f))
    assertTrue(JFloat.isFinite(Float.MinValue))
    assertTrue(JFloat.isFinite(Float.MaxValue))
    assertTrue(JFloat.isFinite(Float.MinPositiveValue))
  }

  @Test def staticHashCodeTest(): Unit = {
    assumeFalse("Hash codes for doubles are different in JS than on the JVM",
        executingInJVM)

    def test(x: Float, expected: Int): Unit =
      assertEquals(expected, JFloat.hashCode(x))

    test(0.0f, 0)
    test(-0.0f, -2147483648)
    test(1234.0f, 1234)
    test(1.5f, 1073217536)
    test(-54.0f, -54)

    test(Float.MinPositiveValue, 916455424)
    test(Float.MinValue, 670040063)
    test(Float.MaxValue, -1477443585)

    test(Float.NaN, 2146959360)
    test(Float.PositiveInfinity, 2146435072)
    test(Float.NegativeInfinity, -1048576)
  }

  // The following tests are only to make sure that things link

  @Test def sum(): Unit = {
    assertEquals(12f, JFloat.sum(5f, 7f), 0f)
  }

  @Test def max(): Unit = {
    assertEquals(7f, JFloat.max(5f, 7f), 0f)
  }

  @Test def min(): Unit = {
    assertEquals(5f, JFloat.min(5f, 7f), 0f)
  }

}
