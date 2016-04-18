/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import java.lang.Math

import org.scalajs.testsuite.utils.Platform._

class MathTest {

  @Test def abs(): Unit = {
    assertEquals(0, Math.abs(0))
    assertEquals(42, Math.abs(42))
    assertEquals(42, Math.abs(-42))
    assertTrue(Math.abs(0.0).equals(0.0))
    assertTrue(Math.abs(-0.0).equals(0.0))
    assertEquals(42.0, Math.abs(42.0), 0.0)
    assertEquals(42.0, Math.abs(-42.0), 0.0)
    assertEquals(Double.PositiveInfinity, Math.abs(Double.PositiveInfinity), 0.0)
    assertEquals(Double.PositiveInfinity, Math.abs(Double.NegativeInfinity), 0.0)
    assertTrue(Math.abs(Double.NaN).isNaN)
    assertEquals(Long.MaxValue, Math.abs(Long.MaxValue))
    assertEquals(Long.MinValue, Math.abs(Long.MinValue))
  }

  @Test def max(): Unit = {
    assertEquals(0, Math.max(0, 0))
    assertEquals(2, Math.max(0, 2))
    assertEquals(2, Math.max(2, 0))
    assertEquals(2.0, Math.max(0.0, 2.0), 0.0)
    assertEquals(2.0, Math.max(2.0, 0.0), 0.0)
    assertTrue(Math.max(0.0, 0.0).equals(0.0))
    assertTrue(Math.max(-0.0, 0.0).equals(0.0))
    assertTrue(Math.max(0.0, -0.0).equals(0.0))
    assertTrue(Math.max(-0.0, -0.0).equals(-0.0))
    assertEquals(Double.PositiveInfinity, Math.max(Double.PositiveInfinity, 0.0), 0.0)
    assertEquals(0.0, Math.max(Double.NegativeInfinity, 0.0), 0.0)
    assertTrue(Math.max(Double.NaN, 0.0).isNaN)
    assertTrue(Math.max(0.0, Double.NaN).isNaN)
    assertEquals(Long.MaxValue, Math.max(Long.MaxValue, 0))
    assertEquals(0L, Math.max(Long.MinValue, 0))
  }

  @Test def min(): Unit = {
    assertEquals(0, Math.min(0, 0))
    assertEquals(0, Math.min(0, 2))
    assertEquals(0, Math.min(2, 0))
    assertEquals(0.0, Math.min(0.0, 2.0), 0.0)
    assertEquals(0.0, Math.min(2.0, 0.0), 0.0)
    assertTrue(Math.min(0.0, 0.0).equals(0.0))
    assertTrue(Math.min(-0.0, 0.0).equals(-0.0))
    assertTrue(Math.min(0.0, -0.0).equals(-0.0))
    assertTrue(Math.min(-0.0, -0.0).equals(-0.0))
    assertEquals(0.0, Math.min(Double.PositiveInfinity, 0.0), 0.0)
    assertEquals(Double.NegativeInfinity, Math.min(Double.NegativeInfinity, 0.0), 0.0)
    assertTrue(Math.min(Double.NaN, 0.0).isNaN)
    assertTrue(Math.min(0.0, Double.NaN).isNaN)
    assertEquals(0L, Math.min(Long.MaxValue, 0))
    assertEquals(Long.MinValue, Math.min(Long.MinValue, 0))
  }

  @Test def cbrt(): Unit = {
    assertTrue(1 / Math.cbrt(-0.0) < 0)
    assertEquals(3.0, Math.cbrt(27.0), 0.0)
    assertEquals(100.0, Math.cbrt(1000000.0), 0.0)
    assertEquals(1000.0, Math.cbrt(1000000000.0), 0.0)
    assertEquals(-100000000.0, Math.cbrt(-1.0E24), 0.0)
    assertEquals(-4039.0E8, Math.cbrt(-65890311319.0E24), 0.0)
  }

  @Test def log1p(): Unit = {
    assertTrue(Math.log1p(-2.0).isNaN)
    assertTrue(Math.log1p(Double.NaN).isNaN)
    assertEquals(0.0, Math.log1p(0.0), 0.0)
  }

  @Test def log10(): Unit = {
    assertTrue(Math.log10(-230.0).isNaN)
    assertTrue(Math.log10(Double.NaN).isNaN)
  }

  @Test def signum_for_Double(): Unit = {
    assertEquals(1.0, Math.signum(234394.2198273), 0.0)
    assertEquals(-1.0, Math.signum(-124937498.58), 0.0)

    assertEquals(0.0, Math.signum(+0.0), 0.0)
    assertTrue(1 / Math.signum(+0.0) > 0)

    assertEquals(-0.0, Math.signum(-0.0), 0.0)
    assertTrue(1 / Math.signum(-0.0) < 0)

    assertTrue(Math.signum(Double.NaN).isNaN)
  }

  @Test def signum_for_Float(): Unit = {
    assertEquals(1.0f, Math.signum(234394.2198273f), 0.0f)
    assertEquals(-1.0f, Math.signum(-124937498.58f), 0.0f)

    assertEquals(0.0f, Math.signum(+0.0f), 0.0f)
    assertTrue(1 / Math.signum(+0.0f) > 0)

    assertEquals(-0.0f, Math.signum(-0.0f), 0.0f)
    assertTrue(1 / Math.signum(-0.0f) < 0)

    assertTrue(Math.signum(Float.NaN).isNaN)
  }

  @Test def nextUp_for_Double(): Unit = {
    assertEquals(Double.PositiveInfinity, Math.nextUp(Double.PositiveInfinity), 0.0)
    assertEquals(Double.MinValue, Math.nextUp(Double.NegativeInfinity), 0.0)
    assertEquals(Double.PositiveInfinity, Math.nextUp(Double.MaxValue), 0.0)
    assertEquals(-1.7976931348623155e+308, Math.nextUp(-Double.MaxValue), 0.0)
    assertEquals(Double.PositiveInfinity, Math.nextUp(-Double.MinValue), 0.0)
    assertEquals(Double.MinPositiveValue, Math.nextUp(0.0), 0.0)
    assertEquals(Double.MinPositiveValue, Math.nextUp(-0.0), 0.0)
    assertEquals(9007199254740992.0, Math.nextUp(9007199254740991.0), 0.0)
    assertEquals(9007199254740994.0, Math.nextUp(9007199254740992.0), 0.0)
    assertEquals(1 + 2.2204460492503130808472633361816E-16, Math.nextUp(1.0), 0.0)
  }

  @Test def nextAfter_for_Double(): Unit = {
    assertTrue(Math.nextAfter(1.0, Double.NaN).isNaN)
    assertTrue(Math.nextAfter(Double.NaN, 1.0).isNaN)
    assertEquals(0.0, Math.nextAfter(0.0, 0.0), 0.0)
    assertEquals(-0.0, Math.nextAfter(0.0, -0.0), 0.0)
    assertEquals(0.0, Math.nextAfter(-0.0, 0.0), 0.0)
    assertEquals(-0.0, Math.nextAfter(-0.0, -0.0), 0.0)
    assertEquals(Double.NegativeInfinity, Math.nextAfter(Double.MinValue, Double.NegativeInfinity), 0.0)
    assertEquals(Double.PositiveInfinity, Math.nextAfter(-Double.MinValue, Double.PositiveInfinity), 0.0)
    assertEquals(Double.MaxValue, Math.nextAfter(Double.PositiveInfinity, Double.NegativeInfinity), 0.0)
    assertEquals(Double.MinValue, Math.nextAfter(Double.NegativeInfinity, Double.PositiveInfinity), 0.0)
    assertEquals(Double.PositiveInfinity, Math.nextAfter(Double.MaxValue, Double.PositiveInfinity), 0.0)
    assertEquals(Double.NegativeInfinity, Math.nextAfter(-Double.MaxValue, Double.NegativeInfinity), 0.0)
    assertEquals(1.0, Math.nextAfter(1.0, 1.0), 0.0)
  }

  @Test def ulp_for_Double(): Unit = {
    assertEquals(4.440892098500626E-16, Math.ulp(3.4), 0.0)
    assertEquals(4.1718496795330275E93, Math.ulp(3.423E109), 0.0)
    assertEquals(Double.MinPositiveValue, Math.ulp(0.0), 0.0)
  }

  @Test def hypot(): Unit = {
    assertEquals(0.0, Math.hypot(0.0, 0.0), 0.01)
    assertEquals(5.0, Math.hypot(3.0, 4.0), 0.01)
    assertTrue(Math.hypot(3.0, Double.NaN).isNaN)
    assertEquals(Double.PositiveInfinity, Math.hypot(Double.NegativeInfinity, 4.0), 0.0)
  }

  @Test def expm1(): Unit = {
    assertTrue(1 / Math.expm1(-0.0) < 0)
    assertEquals(0.0, Math.expm1(-0.0), 0.01)
    assertEquals(19.085536923187668, Math.expm1(3.0), 0.01)
    assertEquals(3269016.3724721107, Math.expm1(15.0), 0.01)
    assertEquals(Double.PositiveInfinity, Math.expm1(1.8E10), 0.0)
    assertEquals(Double.PositiveInfinity, Math.expm1(Double.PositiveInfinity), 0.0)
    assertEquals(-1.0, Math.expm1(Double.NegativeInfinity), 0.01)
    assertEquals(4.9E-324, Math.expm1(4.9E-324), 0.01)
  }

  @Test def sinh(): Unit = {
    assertEquals(Double.NegativeInfinity, Math.sinh(-1234.56), 0.0)
    assertEquals(Double.PositiveInfinity, Math.sinh(1234.56), 0.0)
    assertEquals(0.0, Math.sinh(0.0), 0.01)
    assertEquals(Double.PositiveInfinity, Math.sinh(Double.PositiveInfinity), 0.0)
  }

  @Test def cosh(): Unit = {
    assertEquals(Double.PositiveInfinity, Math.cosh(-1234.56), 0.0)
    assertEquals(Double.PositiveInfinity, Math.cosh(1234.56), 0.0)
    assertEquals(1.0, Math.cosh(-0.0), 0.01)
    assertEquals(Double.PositiveInfinity, Math.cosh(Double.PositiveInfinity), 0.0)
  }

  @Test def tanh(): Unit = {
    assertEquals(-1.0, Math.tanh(-1234.56), 0.01)
    assertEquals(-1.0, Math.tanh(-120.56), 0.01)
    assertEquals(1.0, Math.tanh(1234.56), 0.01)
    assertEquals(0.0, Math.tanh(0.0), 0.01)
    assertEquals(1.0, Math.tanh(Double.PositiveInfinity), 0.01)
    assertEquals(-1.0, Math.tanh(Double.NegativeInfinity), 0.01)
  }

  @Test def rint_for_Double(): Unit = {
    // js.Math.round() is buggy on Rhino
    assumeFalse("Assumed not executing in Rhino", executingInRhino)

    import Math.rint

    def isPosZero(x: Double): Boolean =
      x == 0.0 && (1.0 / x) == Double.PositiveInfinity

    def isNegZero(x: Double): Boolean =
      x == 0.0 && (1.0 / x) == Double.NegativeInfinity

    // Specials
    assertTrue(isPosZero(rint(+0.0)))
    assertTrue(isNegZero(rint(-0.0)))
    assertEquals(Double.PositiveInfinity, rint(Double.PositiveInfinity), 0.0)
    assertEquals(Double.NegativeInfinity, rint(Double.NegativeInfinity), 0.0)
    assertTrue(rint(Double.NaN).isNaN)

    // Positive values
    assertTrue(isPosZero(rint(0.1)))
    assertTrue(isPosZero(rint(0.5)))
    assertEquals(1.0, rint(0.5000000000000001), 0.0)
    assertEquals(1.0, rint(0.999), 0.0)
    assertEquals(1.0, rint(1.4999999999999998), 0.0)
    assertEquals(2.0, rint(1.5), 0.0)
    assertEquals(2.0, rint(2.0), 0.0)
    assertEquals(2.0, rint(2.1), 0.0)
    assertEquals(2.0, rint(2.5), 0.0)
    assertEquals(Double.MaxValue, rint(Double.MaxValue), 0.0)
    assertEquals(4503599627370496.0, rint(4503599627370495.5), 0.0) // MaxSafeInt / 2

    // Negative values
    assertTrue(isNegZero(rint(-0.1)))
    assertTrue(isNegZero(rint(-0.5)))
    assertEquals(-1.0, rint(-0.5000000000000001), 0.0)
    assertEquals(-1.0, rint(-0.999), 0.0)
    assertEquals(-1.0, rint(-1.4999999999999998), 0.0)
    assertEquals(-2.0, rint(-1.5), 0.0)
    assertEquals(-2.0, rint(-2.0), 0.0)
    assertEquals(-2.0, rint(-2.1), 0.0)
    assertEquals(-2.0, rint(-2.5), 0.0)
    assertEquals(Double.MinValue, rint(Double.MinValue), 0.0)
    assertEquals(-4503599627370496.0, rint(-4503599627370495.5), 0.0) // -MaxSafeInt / 2
  }
}
