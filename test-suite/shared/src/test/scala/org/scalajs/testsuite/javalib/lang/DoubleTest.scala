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

import java.lang.{Double => JDouble}

import scala.util.Try

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform.executingInJVM

class DoubleTest {

  @Test def proper_equals(): Unit = {
    assertTrue(0.0.equals(0.0))
    assertTrue((-0.0).equals(-0.0))
    assertFalse(0.0.equals(-0.0))
    assertTrue(Double.NaN.equals(Double.NaN))
  }

  @Test def hashCodeTest(): Unit = {
    def hashCodeNotInlined(x: Any): Int = {
      var y = x // do not inline
      y.hashCode
    }

    def test(x: Double, expected: Int): Unit = {
      assertEquals(expected, x.hashCode)
      assertEquals(expected, hashCodeNotInlined(x))
    }

    if (!executingInJVM) {
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
  }

  @Test def toString_with_integer_values_when_an_integer(): Unit = {
    if (executingInJVM) {
      assertEquals("0.0", 0.0.toString)
      assertEquals("-0.0", (-0.0).toString)
    } else {
      assertEquals("0", 0.0.toString)
      assertEquals("0", (-0.0).toString)
    }
    assertEquals("NaN", Double.NaN.toString)
    assertEquals("Infinity", Double.PositiveInfinity.toString)
    assertEquals("-Infinity", Double.NegativeInfinity.toString)
    if (executingInJVM) {
      assertEquals("5.0", 5.0.toString)
      assertEquals("-5.0", (-5.0).toString)
    } else {
      assertEquals("5", 5.0.toString)
      assertEquals("-5", (-5.0).toString)
    }
    assertEquals("1.2", 1.2.toString)
  }

  @Test def toHexStringTest(): Unit = {
    import java.lang.Double.toHexString

    assertEquals("0x0.0p0", toHexString(0.0))
    assertEquals("-0x0.0p0", toHexString(-0.0))
    assertEquals("NaN", toHexString(Double.NaN))
    assertEquals("Infinity", toHexString(Double.PositiveInfinity))
    assertEquals("-Infinity", toHexString(Double.NegativeInfinity))
    assertEquals("0x1.0p0", toHexString(1.0))
    assertEquals("-0x1.0p0", toHexString(-1.0))
    assertEquals("0x1.0p1", toHexString(2.0))
    assertEquals("0x1.8p1", toHexString(3.0))
    assertEquals("0x1.0p-1", toHexString(0.5))
    assertEquals("0x1.0p-2", toHexString(0.25))
    assertEquals("0x1.00204p3", toHexString(8.003936767578125))
    assertEquals("0x0.00204p-1022", toHexString(1.094949828138e-311))
    assertEquals("0x1.fffffffffffffp1023", toHexString(Double.MaxValue))
    assertEquals("0x1.0p-1022", toHexString(java.lang.Double.MIN_NORMAL))
    assertEquals("0x0.fffffffffffffp-1022", toHexString(2.225073858507201E-308))
    assertEquals("0x0.0000000000001p-1022", toHexString(Double.MinPositiveValue))
  }

  @Test def should_parse_strings(): Unit = {
    assertEquals(0.0, "0.0".toDouble, 0.0)
    assertTrue("NaN".toDouble.isNaN)
    assertTrue(Try("asdf".toDouble).isFailure)

    def test(s: String, v: Double): Unit = {
      assertEquals(v, JDouble.parseDouble(s), 0.01)
      assertEquals(v, JDouble.valueOf(s).doubleValue(), 0.01)
      assertEquals(v, new JDouble(s).doubleValue(), 0.01)
    }

    test("0", 0.0)
    test("5.3", 5.3)
    test("127e2", 12700.0)
    test("127E-2", 1.27)
    test("1E+1", 10)
    test("-123.4", -123.4)
    test("65432.1", 65432.10)
    test("-87654.321", -87654.321)
    test("+.3f", 0.3)
  }

  @Test def should_reject_invalid_strings_when_parsing(): Unit = {
    def test(s: String): Unit =
      expectThrows(classOf[NumberFormatException], JDouble.parseDouble(s))

    test("4.3.5")
    test("4e3.5")
    test("hello world")
    test("--4")
    test("4E-3.2")
  }

  @Test def compareTo(): Unit = {
    def compare(x: Double, y: Double): Int =
      new JDouble(x).compareTo(new JDouble(y))

    assertTrue(compare(0.0, 5.5) < 0)
    assertTrue(compare(10.5, 10.2) > 0)
    assertTrue(compare(-2.1, -1.0) < 0)
    assertEquals(0, compare(3.14, 3.14))

    // From compareTo's point of view, NaN is equal to NaN
    assertEquals(0, compare(Double.NaN, Double.NaN))

    // And -0.0 < 0.0
    assertTrue(compare(-0.0, 0.0) < 0)
    assertTrue(compare(0.0, -0.0) > 0)
  }

  @Test def should_be_a_Comparable(): Unit = {
    def compare(x: Any, y: Any): Int =
      x.asInstanceOf[Comparable[Any]].compareTo(y)

    assertTrue(compare(0.0, 5.5) < 0)
    assertTrue(compare(10.5, 10.2) > 0)
    assertTrue(compare(-2.1, -1.0) < 0)
    assertEquals(0, compare(3.14, 3.14))

    // From compareTo's point of view, NaN is equal to NaN
    assertEquals(0, compare(Double.NaN, Double.NaN))

    // And -0.0 < 0.0
    assertTrue(compare(-0.0, 0.0) < 0)
    assertTrue(compare(0.0, -0.0) > 0)
  }

  @Test def `isInfinite_- #515`(): Unit = {
    assertTrue(Double.PositiveInfinity.isInfinite)
    assertTrue(Double.NegativeInfinity.isInfinite)
    assertTrue((1.0/0).isInfinite)
    assertTrue((-1.0/0).isInfinite)
    assertFalse((0.0).isInfinite)
  }

  @Test def isNaNTest(): Unit = {
    def f(v: Double): Boolean = {
      var v2 = v // do not inline
      v2.isNaN
    }

    assertTrue(f(Double.NaN))

    assertFalse(f(Double.PositiveInfinity))
    assertFalse(f(Double.NegativeInfinity))
    assertFalse(f(1.0 / 0))
    assertFalse(f(-1.0 / 0))
    assertFalse(f(0.0))
    assertFalse(f(3.0))
    assertFalse(f(-1.5))
  }

  @Test def longBitsToDouble(): Unit = {
    def isZero(v: Double, neg: Boolean): Boolean = {
      (v == 0.0) && (1 / v == (
          if (neg) Double.NegativeInfinity
          else Double.PositiveInfinity))
    }

    import JDouble.{longBitsToDouble => f}

    // Specials
    assertEquals(Double.PositiveInfinity, f(0x7ff0000000000000L), 0.0)
    assertEquals(Double.NegativeInfinity, f(0xfff0000000000000L), 0.0)
    assertTrue(isZero(f(0x0000000000000000L), false))
    assertTrue(isZero(f(0x8000000000000000L), true))
    assertTrue(f(0x7ff8000000000000L).isNaN) // canonical NaN

    // Non-canonical NaNs
    assertTrue(f(0x7ff0000000000001L).isNaN) // smallest positive NaN
    assertTrue(f(0x7ff15ab515d3cca1L).isNaN) // an arbitrary positive NaN
    assertTrue(f(0x7fffffffffffffffL).isNaN) // largest positive NaN
    assertTrue(f(0xfff0000000000001L).isNaN) // smallest negative NaN
    assertTrue(f(0xfff15ab515d3cca1L).isNaN) // an arbitrary negative NaN
    assertTrue(f(0xffffffffffffffffL).isNaN) // largest negative NaN

    // Normal forms
    assertEquals(2.2250738585072014e-308, f(0x0010000000000000L), 0.0)  // smallest pos normal form
    assertEquals(1.7976931348623157e308, f(0x7fefffffffffffffL), 0.0)   // largest pos normal form
    assertEquals(1.8790766677624813e63, f(0x4d124568bc6584caL), 0.0)    // an arbitrary pos normal form
    assertEquals(-2.2250738585072014e-308, f(0x8010000000000000L), 0.0) // smallest neg normal form
    assertEquals(-1.7976931348623157e308, f(0xffefffffffffffffL), 0.0)  // largest neg normal form
    assertEquals(-1.8790766677624813e63, f(0xcd124568bc6584caL), 0.0)   // an arbitrary neg normal form

    // Subnormal forms
    assertEquals(Double.MinPositiveValue, f(0x0000000000000001L), 0.0)  // smallest pos subnormal form
    assertEquals(2.225073858507201e-308, f(0x000fffffffffffffL), 0.0)   // largest pos subnormal form
    assertEquals(1.719471609939382e-308, f(0x000c5d44ae45cb60L), 0.0)   // an arbitrary pos subnormal form
    assertEquals(-Double.MinPositiveValue, f(0x8000000000000001L), 0.0) // smallest neg subnormal form
    assertEquals(-2.225073858507201e-308, f(0x800fffffffffffffL), 0.0)  // largest neg subnormal form
    assertEquals(-1.719471609939382e-308, f(0x800c5d44ae45cb60L), 0.0)  // an arbitrary neg subnormal form
  }

  @Test def doubleToLongBits(): Unit = {
    import JDouble.{doubleToLongBits => f}

    // Specials
    assertEquals(0x7ff0000000000000L, f(Double.PositiveInfinity))
    assertEquals(0xfff0000000000000L, f(Double.NegativeInfinity))
    assertEquals(0x0000000000000000L, f(0.0))
    assertEquals(0x8000000000000000L, f(-0.0))
    assertEquals(0x7ff8000000000000L, f(Double.NaN)) // canonical NaN

    // Normal forms
    assertEquals(0x0010000000000000L, f(2.2250738585072014e-308))  // smallest pos normal form
    assertEquals(0x7fefffffffffffffL, f(1.7976931348623157e308))   // largest pos normal form
    assertEquals(0x4d124568bc6584caL, f(1.8790766677624813e63))    // an arbitrary pos normal form
    assertEquals(0x8010000000000000L, f(-2.2250738585072014e-308)) // smallest neg normal form
    assertEquals(0xffefffffffffffffL, f(-1.7976931348623157e308))  // largest neg normal form
    assertEquals(0xcd124568bc6584caL, f(-1.8790766677624813e63))   // an arbitrary neg normal form

    // Subnormal forms
    assertEquals(0x0000000000000001L, f(Double.MinPositiveValue))  // smallest pos subnormal form
    assertEquals(0x000fffffffffffffL, f(2.225073858507201e-308))   // largest pos subnormal form
    assertEquals(0x000c5d44ae45cb60L, f(1.719471609939382e-308))   // an arbitrary pos subnormal form
    assertEquals(0x8000000000000001L, f(-Double.MinPositiveValue)) // smallest neg subnormal form
    assertEquals(0x800fffffffffffffL, f(-2.225073858507201e-308))  // largest neg subnormal form
    assertEquals(0x800c5d44ae45cb60L, f(-1.719471609939382e-308))  // an arbitrary neg subnormal form
  }
}
