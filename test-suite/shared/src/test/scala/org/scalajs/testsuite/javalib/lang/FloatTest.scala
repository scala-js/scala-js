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

import java.lang.{Float => JFloat}

import scala.util.Try

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform.executingInJVM

class FloatTest {

  @Test def proper_equals(): Unit = {
    assertTrue(0.0f.equals(0.0f))
    assertTrue((-0.0f).equals(-0.0f))
    assertFalse(0.0f.equals(-0.0f))
    assertTrue(Float.NaN.equals(Float.NaN))
  }

  @Test def hashCodeTest(): Unit = {
    def hashCodeNotInlined(x: Any): Int = {
      var y = x // do not inline
      y.hashCode
    }

    def test(x: Float, expected: Int): Unit = {
      assertEquals(expected, x.hashCode)
      assertEquals(expected, hashCodeNotInlined(x))
    }

    if (!executingInJVM) {
      test(0.0f, 0)
      test(-0.0f, -2147483648)
      test(1234.0f, 1234)
      test(1.5f, 1073217536)
      test(-54f, -54)

      test(Float.MinPositiveValue, 916455424)
      test(Float.MinValue, 670040063)
      test(Float.MaxValue, -1477443585)

      test(Float.NaN, 2146959360)
      test(Float.PositiveInfinity, 2146435072)
      test(Float.NegativeInfinity, -1048576)
    }
  }

  @Test def toString_with_integer_values_when_an_integer(): Unit = {
    if (executingInJVM) {
      assertEquals("0.0", 0.0f.toString)
      assertEquals("-0.0", (-0.0f).toString)
    } else {
      assertEquals("0", 0.0f.toString)
      assertEquals("0", (-0.0f).toString)
    }
    assertEquals("NaN", Float.NaN.toString)
    assertEquals("Infinity", Float.PositiveInfinity.toString)
    assertEquals("-Infinity", Float.NegativeInfinity.toString)
    if (executingInJVM) {
      assertEquals("5.0", 5.0f.toString)
      assertEquals("-5.0", (-5.0f).toString)
    } else {
      assertEquals("5", 5.0f.toString)
      assertEquals("-5", (-5.0f).toString)
    }
    // We need to explicitly cut the string here, since floats are
    // represented by doubles (but the literal is emitted as
    // float). Therefore there may be some imprecision. This is
    // documented as semantic difference.
    assertEquals("1.2", 1.2f.toString.substring(0,3))
  }

  @Test def toHexStringTest(): Unit = {
    import java.lang.Float.toHexString

    assertEquals("NaN", toHexString(Float.NaN))
    assertEquals("Infinity", toHexString(Float.PositiveInfinity))
    assertEquals("-Infinity", toHexString(Float.NegativeInfinity))
    assertEquals("0x0.0p0", toHexString(0.0f))
    assertEquals("-0x0.0p0", toHexString(-0.0f))
    assertEquals("0x1.0p0", toHexString(1.0f))
    assertEquals("-0x1.0p0", toHexString(-1.0f))
    assertEquals("0x1.0p1", toHexString(2.0f))
    assertEquals("0x1.8p1", toHexString(3.0f))
    assertEquals("0x1.0p-1", toHexString(0.5f))
    assertEquals("0x1.0p-2", toHexString(0.25f))
    assertEquals("0x1.00204p3", toHexString(8.003937f))
    assertEquals("0x0.00204p-126", toHexString(5.785e-42f))
    assertEquals("0x1.fffffep127", toHexString(Float.MaxValue))
    assertEquals("0x1.0p-126", toHexString(java.lang.Float.MIN_NORMAL))
    assertEquals("0x0.fffffep-126", toHexString(1.1754942E-38f))
    assertEquals("0x0.000002p-126", toHexString(Float.MinPositiveValue))
  }

  @Test def should_parse_strings(): Unit = {
    assertEquals(0.0f, "0.0".toFloat, 0.0f)
    assertTrue("NaN".toFloat.isNaN)
    assertTrue(Try("asdf".toFloat).isFailure)

    def test(s: String, v: Float): Unit = {
      assertEquals(v, JFloat.parseFloat(s), 0.01f)
      assertEquals(v, JFloat.valueOf(s).floatValue(), 0.01f)
      assertEquals(v, new JFloat(s).floatValue(), 0.01f)
    }

    if (executingInJVM) {
      test("0.0", 0.0f)
    } else {
      test("0", 0.0f)
    }
    test("5.3", 5.3f)
    test("127e2", 12700.0f)
    test("127E-2", 1.27f)
    test("1E+1", 10f)
    test("-123.4", -123.4f)
    test("65432.1", 65432.10f)
    test("-87654.321", -87654.321f)
    test("+.3f", 0.3f)
  }

  @Test def should_reject_invalid_strings_when_parsing(): Unit = {
    def test(s: String): Unit =
      expectThrows(classOf[NumberFormatException], JFloat.parseFloat(s))

    test("4.3.5")
    test("4e3.5")
    test("hello world")
    test("--4")
    test("4E-3.2")
  }

  @Test def compareTo(): Unit = {
    def compare(x: Float, y: Float): Int =
      new JFloat(x).compareTo(new JFloat(y))

    assertTrue(compare(0.0f, 5.5f) < 0)
    assertTrue(compare(10.5f, 10.2f) > 0)
    assertTrue(compare(-2.1f, -1.0f) < 0)
    assertEquals(0, compare(3.14f, 3.14f), 0.0f)

    // From compareTo's point of view, NaN is equal to NaN
    assertEquals(0, compare(Float.NaN, Float.NaN), 0.0f)

    // And -0.0 < 0.0
    assertTrue(compare(-0.0f, 0.0f) < 0)
    assertTrue(compare(0.0f, -0.0f) > 0)
  }

  @Test def should_be_a_Comparable(): Unit = {
    def compare(x: Any, y: Any): Int =
      x.asInstanceOf[Comparable[Any]].compareTo(y)

    assertTrue(compare(0.0f, 5.5f) < 0)
    assertTrue(compare(10.5f, 10.2f) > 0)
    assertTrue(compare(-2.1f, -1.0f) < 0)
    assertEquals(0, compare(3.14f, 3.14f), 0.0f)

    // From compareTo's point of view, NaN is equal to NaN
    assertEquals(0, compare(Float.NaN, Float.NaN), 0.0f)

    // And -0.0 < 0.0
    assertTrue(compare(-0.0f, 0.0f) < 0)
    assertTrue(compare(0.0f, -0.0f) > 0)
  }

  @Test def `isInfinite_- #515`(): Unit = {
    assertTrue(Float.PositiveInfinity.isInfinite)
    assertTrue(Float.NegativeInfinity.isInfinite)
    assertTrue((1f/0).isInfinite)
    assertTrue((-1f/0).isInfinite)
    assertFalse(0f.isInfinite)
  }

  @Test def isNaNTest(): Unit = {
    def f(v: Float): Boolean = {
      var v2 = v // do not inline
      v2.isNaN
    }

    assertTrue(f(Float.NaN))

    assertFalse(f(Float.PositiveInfinity))
    assertFalse(f(Float.NegativeInfinity))
    assertFalse(f(1f / 0))
    assertFalse(f(-1f / 0))
    assertFalse(f(0f))
    assertFalse(f(3f))
    assertFalse(f(-1.5f))
  }

  @Test def intBitsToFloat(): Unit = {
    def isZero(v: Float, neg: Boolean): Boolean = {
      (v == 0.0f) && (1 / v == (
          if (neg) Float.NegativeInfinity
          else Float.PositiveInfinity))
    }

    import JFloat.{intBitsToFloat => f}

    // Specials
    assertEquals(Float.PositiveInfinity, f(0x7f800000), 0.0f)
    assertEquals(Float.NegativeInfinity, f(0xff800000), 0.0f)
    assertTrue(isZero(f(0x00000000), false))
    assertTrue(isZero(f(0x80000000), true))
    assertTrue(f(0x7fc00000).isNaN) // canonical NaN

    // Non-canonical NaNs
    assertTrue(f(0x7f800001).isNaN) // smallest positive NaN
    assertTrue(f(0x7f915ab5).isNaN) // an arbitrary positive NaN
    assertTrue(f(0x7fffffff).isNaN) // largest positive NaN
    assertTrue(f(0xff800001).isNaN) // smallest negative NaN
    assertTrue(f(0xff915ab5).isNaN) // an arbitrary negative NaN
    assertTrue(f(0xffffffff).isNaN) // largest negative NaN

    // Normal forms
    assertEquals(1.17549435e-38f, f(0x00800000), 0.0f)  // smallest pos normal form
    assertEquals(3.4028234e38f, f(0x7f7fffff), 0.0f)    // largest pos normal form
    assertEquals(1.53376384e8f, f(0x4d124568), 0.0f)    // an arbitrary pos normal form
    assertEquals(-1.17549435e-38f, f(0x80800000), 0.0f) // smallest neg normal form
    assertEquals(-3.4028234e38f, f(0xff7fffff), 0.0f)   // largest neg normal form
    assertEquals(-1.53376384e8f, f(0xcd124568), 0.0f)   // an arbitrary neg normal form

    // Subnormal forms
    assertEquals(Float.MinPositiveValue, f(0x00000001), 0.0f)  // smallest pos subnormal form
    assertEquals(1.1754942e-38f, f(0x007fffff), 0.0f)          // largest pos subnormal form
    assertEquals(1.1421059e-38f, f(0x007c5d44), 0.0f)          // an arbitrary pos subnormal form
    assertEquals(-Float.MinPositiveValue, f(0x80000001), 0.0f) // smallest neg subnormal form
    assertEquals(-1.1754942e-38f, f(0x807fffff), 0.0f)         // largest neg subnormal form
    assertEquals(-1.1421059e-38f, f(0x807c5d44), 0.0f)         // an arbitrary neg subnormal form
  }

  @Test def floatToIntBits(): Unit = {
    import JFloat.{floatToIntBits => f}

    // Specials
    assertEquals(0x7f800000, f(Float.PositiveInfinity))
    assertEquals(0xff800000, f(Float.NegativeInfinity))
    assertEquals(0x00000000, f(0.0f))
    assertEquals(0x80000000, f(-0.0f))
    assertEquals(0x7fc00000, f(Float.NaN)) // canonical NaN

    // Normal forms
    assertEquals(0x00800000, f(1.17549435e-38f))  // smallest pos normal form
    assertEquals(0x7f7fffff, f(3.4028234e38f))    // largest pos normal form
    assertEquals(0x4d124568, f(1.53376384e8f))    // an arbitrary pos normal form
    assertEquals(0x80800000, f(-1.17549435e-38f)) // smallest neg normal form
    assertEquals(0xff7fffff, f(-3.4028234e38f))   // largest neg normal form
    assertEquals(0xcd124568, f(-1.53376384e8f))   // an arbitrary neg normal form

    // Subnormal forms
    assertEquals(0x00000001, f(Float.MinPositiveValue))  // smallest pos subnormal form
    assertEquals(0x007fffff, f(1.1754942e-38f))          // largest pos subnormal form
    assertEquals(0x007c5d44, f(1.1421059e-38f))          // an arbitrary pos subnormal form
    assertEquals(0x80000001, f(-Float.MinPositiveValue)) // smallest neg subnormal form
    assertEquals(0x807fffff, f(-1.1754942e-38f))         // largest neg subnormal form
    assertEquals(0x807c5d44, f(-1.1421059e-38f))         // an arbitrary neg subnormal form
  }
}
