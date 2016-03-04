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

import org.scalajs.testsuite.utils.Requires

object FloatJSTest extends Requires.StrictFloats

class FloatJSTest {

  @noinline def froundNotInlined(x: Double): Float =
    x.toFloat

  @Test def fround_for_special_values(): Unit = {
    assertTrue(froundNotInlined(Double.NaN).isNaN)
    assertEquals(Double.PositiveInfinity, 1 / froundNotInlined(0.0).toDouble)
    assertEquals(Double.NegativeInfinity, 1 / froundNotInlined(-0.0).toDouble)
    assertEquals(Float.PositiveInfinity, froundNotInlined(Double.PositiveInfinity))
    assertEquals(Float.NegativeInfinity, froundNotInlined(Double.NegativeInfinity))
  }

  @Test def fround_overflows(): Unit = {
    assertEquals(Double.PositiveInfinity, froundNotInlined(1e200))
    assertEquals(Double.NegativeInfinity, froundNotInlined(-1e200))
  }

  @Test def fround_underflows(): Unit = {
    assertEquals(Double.PositiveInfinity, 1 / froundNotInlined(1e-300).toDouble)
    assertEquals(Double.NegativeInfinity, 1 / froundNotInlined(-1e-300).toDouble)
  }

  @Test def fround_normal_cases(): Unit = {
    def test(input: Double, expected: Double): Unit =
      assertEquals(expected, input.toFloat.toDouble)

    // From MDN documentation
    test(0.0, 0.0)
    test(1.0, 1.0)
    test(1.5, 1.5)
    test(1.337, 1.3370000123977661)
    test(-4.3, -4.300000190734863)

    // Some bounds
    test(Float.MinPositiveValue.toDouble, Float.MinPositiveValue.toDouble)
    test(Float.MaxValue.toDouble, Float.MaxValue.toDouble)
    test(Float.MinValue.toDouble, Float.MinValue.toDouble)

    // Randomly generated Doubles
    test(2.705609035558863E20, 2.7056090763400262E20)
    test(-1.447710531503027E15, -1.447710532042752E15)
    test(-5.1970024617732836E13, -5.1970022834176E13)
    test(1.627661085098256E31, 1.6276610930768024E31)
    test(-3.7731947682593834E-32, -3.7731946313230934E-32)
    test(34.48229849163326, 34.4822998046875)
    test(26.62034396181652, 26.620344161987305)
    test(8.198435190113375E-24, 8.198434961596576E-24)
    test(-6.079928908440255E-23, -6.079928963558556E-23)
    test(3.3756949828462674E-13, 3.37569490589662E-13)
    test(-1.2599049874324274E33, -1.2599049641449257E33)
    test(6.08574575776438E-10, 6.085745796191588E-10)
    test(1.973497969450596E-21, 1.973498047135062E-21)
  }

  @Test def Int_should_be_cast_to_Float_when_comparing_to_Float_issue_1878(): Unit = {
    val intMax: Int = Int.MaxValue
    val float: Float = (Int.MaxValue - 1).toFloat

    assertTrue(intMax == float)
    assertFalse(intMax != float)
    assertFalse(intMax < float)
    assertTrue(intMax <= float)
    assertFalse(intMax > float)
    assertTrue(intMax >= float)

    assertTrue(float == intMax)
    assertFalse(float != intMax)
    assertFalse(float < intMax)
    assertTrue(float <= intMax)
    assertFalse(float > intMax)
    assertTrue(float >= intMax)
  }
}
