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
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertExtensions.assertExactEquals
import org.scalajs.testsuite.utils.Platform._

class DoubleTest {
  @Test
  def `toInt`(): Unit = {
    @inline
    def test(x: Double, expected: Int): Unit =
      assertEquals(expected, x.toInt)

    // Specials
    test(+0.0, 0)
    test(-0.0, 0)
    test(Double.PositiveInfinity, Int.MaxValue)
    test(Double.NegativeInfinity, Int.MinValue)
    test(Double.NaN, 0)

    // Positive numbers
    test(0.3, 0)
    test(0.7, 0)
    test(1.2, 1)
    test(5e12, Int.MaxValue)
    test(2147483646, 2147483646)
    test(2147483646.999, 2147483646)
    test(2147483512.546, 2147483512)
    test(65.67, 65)

    // Negative numbers
    test(-0.3, 0)
    test(-0.7, 0)
    test(-1.2, -1)
    test(-5e12, Int.MinValue)
    test(-2147483647.9999, -2147483647)
    test(-2147483565.123, -2147483565)
    test(-65.67, -65)
  }

  @Test
  def toFloat(): Unit = {
    // This is the closest we get to directly testing our `Math.fround` polyfill

    @noinline
    def test(expected: Float, value: Double): Unit =
      assertExactEquals(s"for value $value", expected, value.toFloat)

    // Values based on the limits of Doubles

    // Normal forms
    test(0.0f, 2.2250738585072014e-308) // smallest pos normal form
    test(Float.PositiveInfinity, 1.7976931348623157e308) // largest pos normal form
    test(1.8790767e23f, 1.8790766677624812e23) // an arbitrary pos normal form
    test(-0.0f, -2.2250738585072014e-308) // smallest neg normal form
    test(Float.NegativeInfinity, -1.7976931348623157e308) // largest neg normal form
    test(-1.8790767e23f, -1.8790766677624812e23) // an arbitrary neg normal form

    // Some corner cases of doubleToLongBits
    test(9.0071993e15f, 9007199254740991.0)
    test(8.9884656e30f, 8.988465674311579e+30)
    test(5.9152608e-27f, 5.915260930833876e-27)
    test(4.4501478e-30f, 4.450147717014403e-30)

    // Subnormal forms (they all underflow)
    test(0.0f, Double.MinPositiveValue) // smallest pos subnormal form
    test(0.0f, 2.225073858507201e-308) // largest pos subnormal form
    test(0.0f, 1.719471609939382e-308) // an arbitrary pos subnormal form
    test(-0.0f, -Double.MinPositiveValue) // smallest neg subnormal form
    test(-0.0f, -2.225073858507201e-308) // largest neg subnormal form
    test(-0.0f, -1.719471609939382e-308) // an arbitrary neg subnormal form

    // Values based on the limits of Floats

    // Around Float.MinPositiveValue.toDouble / 2.0
    test(0.0f, 7.006492321624084e-46) // just below
    test(0.0f, 7.006492321624085e-46) // Float.MinPositiveValue.toDouble / 2.0
    test(Float.MinPositiveValue, 7.006492321624087e-46) // just above

    // Around Float.MinPositiveValue
    test(Float.MinPositiveValue, 1.40129e-45)
    test(Float.MinPositiveValue, 1.401298464324812e-45)
    test(Float.MinPositiveValue, 1.401298464324832e-45)
    test(Float.MinPositiveValue, 1.40131e-45)

    // Around 3.4e-40f, which is a subnormal value
    test(3.4e-40f, 3.39999848996058e-40)
    test(3.4e-40f, 3.39999848996059e-40)
    test(3.4e-40f, 3.3999984899606e-40)

    // Around 3.4000054964529118e-40, which is the midpoint between 3.4e-40f and 3.40001e-40f
    test(3.4e-40f, 3.4000054964529114e-40)
    test(3.4e-40f, 3.4000054964529118e-40) // even is downwards
    test(3.40001e-40f, 3.400005496452912e-40)

    // Around 3.400019509437555e-40, which is the midpoint between 3.40001e-40f and 3.40003e-40f
    test(3.40001e-40f, 3.4000195094375546e-40)
    test(3.40003e-40f, 3.400019509437555e-40) // even is upwards
    test(3.40003e-40f, 3.4000195094375554e-40)

    // Around 1.1754942807573643e-38, which is the midpoint between max-subnormal and min-normal
    test(1.1754942e-38f, 1.1754942807573642e-38)
    test(1.17549435e-38f, 1.1754942807573643e-38) // even is upwards (it's min-normal)
    test(1.17549435e-38f, 1.1754942807573644e-38)

    // Around 2.3509886e-38f, which is the max value with ulp == MinPosValue
    test(2.3509886e-38f, 2.3509885615147283e-38)
    test(2.3509886e-38f, 2.3509885615147286e-38)
    test(2.3509886e-38f, 2.3509885615147283e-38)

    // Around 2.3509887e-38f, which is the min value with ulp != MinPosValue
    test(2.3509887e-38f, 2.3509887016445748e-38)
    test(2.3509887e-38f, 2.350988701644575e-38)
    test(2.3509887e-38f, 2.3509887016445755e-38)

    // Around 3.400000214576721, which is the midpoint between 3.4f and 3.4000003f (normals)
    test(3.4f, 3.4000002145767207)
    test(3.4f, 3.400000214576721) // even is downwards
    test(3.4000003f, 3.4000002145767216)

    // Around 3.4000004529953003, which is the midpoint between 3.4000003f and 3.4000006f (normals)
    test(3.4000003f, 3.4000004529953)
    test(3.4000006f, 3.4000004529953003) // even is upwards
    test(3.4000006f, 3.4000004529953007)

    // Around 3.4028235677973366e38, which is the midpoint between Float.MaxValue and Infinity
    test(Float.MaxValue, 3.4028235677973362e38)
    test(Float.PositiveInfinity, 3.4028235677973366e38)
    test(Float.PositiveInfinity, 3.402823567797337e38)
  }

  @Test
  def toFloatNoLoss(): Unit = {
    // This is the closest we get to directly testing our `Math.fround` polyfill

    /* Whether we have accurate floats or not, `toFloat` must be exact when the
     * input is already a float.
     */

    @noinline
    def toFloatNoInline(value: Double): Float = value.toFloat

    @noinline
    def test(value: Float): Unit =
      assertExactEquals(s"for value $value", value, toFloatNoInline(value.toDouble))

    // Specials
    test(+0.0f)
    test(-0.0f)
    test(Float.PositiveInfinity)
    test(Float.NegativeInfinity)
    test(Float.NaN)

    // Other corner cases

    test(Float.MinPositiveValue)
    test(-Float.MinPositiveValue)
    test(1.1754942e-38f) // max subnormal value
    test(-1.1754942e-38f)
    test(1.17549435e-38f) // min normal value
    test(-1.17549435e-38f)
    test(2.3509886e-38f) // max value with ulp == MinPosValue
    test(-2.3509886e-38f)
    test(2.3509887e-38f) // min value with ulp != MinPosValue
    test(-2.3509887e-38f)
    test(Float.MaxValue)
    test(-Float.MaxValue)

    // Some normal values

    test(3.4f)
    test(-3.4f)
    test(3.423e36f)
    test(-3.423e36f)

    // Some subnormal values

    test(3.4e-40f)
    test(-3.4e-40f)
    test(3.42e-43f)
    test(-3.42e-43f)
  }

  @Test
  def differentialTestDivide(): Unit = {
    /* The optimizer rewrites divisions by powers of 2 to multiplications.
     * Test that it does not do anything wrong with them with some sort of
     * differntial testing.
     */

    import Double.{NaN, MinPositiveValue, PositiveInfinity, NegativeInfinity}
    import java.lang.Double.{MIN_NORMAL => MinNormal}

    val numerators: List[Double] = List(
      NaN,
      PositiveInfinity,
      NegativeInfinity,
      +0.0,
      -0.0,
      1.0,
      -1.0,
      1.2564,
      -6.54321,
      MinPositiveValue,
      -MinPositiveValue,
      MinNormal,
      -MinNormal,
      2.225073858507201e-308, // largest subnormal
      -2.225073858507201e-308,
      6.0995758e-316, // some subnormal
      -6.0995758e-316,
      Double.MaxValue,
      Double.MinValue,
      1.7754993924788018e308, // value close to the max value
      -1.7754993924788018e308
    )

    @noinline def hide(x: Double): Double = x

    @inline
    def test(n: Double, d: Double): Unit =
      assertExactEquals(n.toString(), n / hide(d), n / d)

    for (n <- numerators) {
      // Specials
      test(n, NaN)
      test(n, PositiveInfinity)
      test(n, NegativeInfinity)
      test(n, +0.0)
      test(n, -0.0)

      // Powers of 2 whose inverse is representable
      test(n, 1.0)
      test(n, -1.0)
      test(n, 2.0)
      test(n, -2.0)
      test(n, 0.5)
      test(n, -0.5)
      test(n, MinNormal)
      test(n, -MinNormal)
      test(n, 256 * MinNormal)
      test(n, -256 * MinNormal)
      test(n, 0.5 * MinNormal) // smallest power of 2 whose inverse is representable
      test(n, -0.5 * MinNormal)
      test(n, 8.98846567431158e307) // largest power of 2
      test(n, -8.98846567431158e307)
      test(n, 1.0715086071862673e301)
      test(n, -1.0715086071862673e301)

      // Powers of 2 whose inverse is not representable (wrongly optimized by GCC)
      if (!usesClosureCompiler) {
        test(n, MinPositiveValue)
        test(n, -MinPositiveValue)
        test(n, 256 * MinPositiveValue)
        test(n, -256 * MinPositiveValue)
        test(n, 0.25 * MinNormal) // largest power of 2 whose inverse is not representable
        test(n, -0.25 * MinNormal)
      }

      // Non-powers of 2
      test(n, 1.2564)
      test(n, -6.54321)
      test(n, 2.225073858507201e-308) // largest subnormal
      test(n, -2.225073858507201e-308)
      test(n, 6.0995758e-316) // some subnormal
      test(n, -6.0995758e-316)
      test(n, Double.MaxValue)
      test(n, -Double.MaxValue)
      test(n, 1.7754993924788018e308) // value close to the max value
      test(n, -1.7754993924788018e308)
    }
  }

  @Test
  def testRemainder(): Unit = {
    /* Double `%` is atypical. It does not correspond to the IEEE-754 notion
     * of remainder/modulo. Instead, it correspond to the common math function
     * `fmod`. Therefore, we have dedicated tests for it, to make sure that
     * our platforms agree on the semantics. They are not much, but they are
     * enough to rule out the naive formula that can sometimes be found on the
     * Web, namely `x - trunc(x / y) * y`.
     */

    def test(expected: Double, n: Double, d: Double): Unit =
      assertExactEquals(expected, n % d)

    // If n is NaN, return NaN
    test(Double.NaN, Double.NaN, Double.NaN)
    test(Double.NaN, Double.NaN, Double.PositiveInfinity)
    test(Double.NaN, Double.NaN, Double.NegativeInfinity)
    test(Double.NaN, Double.NaN, +0.0)
    test(Double.NaN, Double.NaN, -0.0)
    test(Double.NaN, Double.NaN, 2.1)
    test(Double.NaN, Double.NaN, 5.5)
    test(Double.NaN, Double.NaN, -151.189)
    test(Double.NaN, Double.NaN, 3.123e-320)
    test(Double.NaN, Double.NaN, 2.253547e-318)

    // If d is NaN, return NaN
    test(Double.NaN, Double.NaN, Double.NaN)
    test(Double.NaN, Double.PositiveInfinity, Double.NaN)
    test(Double.NaN, Double.NegativeInfinity, Double.NaN)
    test(Double.NaN, +0.0, Double.NaN)
    test(Double.NaN, -0.0, Double.NaN)
    test(Double.NaN, 2.1, Double.NaN)
    test(Double.NaN, 5.5, Double.NaN)
    test(Double.NaN, -151.189, Double.NaN)
    test(Double.NaN, 3.123e-320, Double.NaN)
    test(Double.NaN, 2.253547e-318, Double.NaN)

    // If n is PositiveInfinity, return NaN
    test(Double.NaN, Double.PositiveInfinity, Double.PositiveInfinity)
    test(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity)
    test(Double.NaN, Double.PositiveInfinity, +0.0)
    test(Double.NaN, Double.PositiveInfinity, -0.0)
    test(Double.NaN, Double.PositiveInfinity, 2.1)
    test(Double.NaN, Double.PositiveInfinity, 5.5)
    test(Double.NaN, Double.PositiveInfinity, -151.189)
    test(Double.NaN, Double.PositiveInfinity, 3.123e-320)
    test(Double.NaN, Double.PositiveInfinity, 2.253547e-318)

    // If n is NegativeInfinity, return NaN
    test(Double.NaN, Double.NegativeInfinity, Double.PositiveInfinity)
    test(Double.NaN, Double.NegativeInfinity, Double.NegativeInfinity)
    test(Double.NaN, Double.NegativeInfinity, +0.0)
    test(Double.NaN, Double.NegativeInfinity, -0.0)
    test(Double.NaN, Double.NegativeInfinity, 2.1)
    test(Double.NaN, Double.NegativeInfinity, 5.5)
    test(Double.NaN, Double.NegativeInfinity, -151.189)
    test(Double.NaN, Double.NegativeInfinity, 3.123e-320)
    test(Double.NaN, Double.NegativeInfinity, 2.253547e-318)

    // If d is PositiveInfinity, return n
    test(+0.0, +0.0, Double.PositiveInfinity)
    test(-0.0, -0.0, Double.PositiveInfinity)
    test(2.1, 2.1, Double.PositiveInfinity)
    test(5.5, 5.5, Double.PositiveInfinity)
    test(-151.189, -151.189, Double.PositiveInfinity)
    test(3.123e-320, 3.123e-320, Double.PositiveInfinity)
    test(2.253547e-318, 2.253547e-318, Double.PositiveInfinity)

    // If d is NegativeInfinity, return n
    test(+0.0, +0.0, Double.NegativeInfinity)
    test(-0.0, -0.0, Double.NegativeInfinity)
    test(2.1, 2.1, Double.NegativeInfinity)
    test(5.5, 5.5, Double.NegativeInfinity)
    test(-151.189, -151.189, Double.NegativeInfinity)
    test(3.123e-320, 3.123e-320, Double.NegativeInfinity)
    test(2.253547e-318, 2.253547e-318, Double.NegativeInfinity)

    // If d is +0.0, return NaN
    test(Double.NaN, +0.0, +0.0)
    test(Double.NaN, -0.0, +0.0)
    test(Double.NaN, 2.1, +0.0)
    test(Double.NaN, 5.5, +0.0)
    test(Double.NaN, -151.189, +0.0)
    test(Double.NaN, 3.123e-320, +0.0)
    test(Double.NaN, 2.253547e-318, +0.0)

    // If d is -0.0, return NaN
    test(Double.NaN, +0.0, -0.0)
    test(Double.NaN, -0.0, -0.0)
    test(Double.NaN, 2.1, -0.0)
    test(Double.NaN, 5.5, -0.0)
    test(Double.NaN, -151.189, -0.0)
    test(Double.NaN, 3.123e-320, -0.0)
    test(Double.NaN, 2.253547e-318, -0.0)

    // If n is +0.0, return n
    test(+0.0, +0.0, 2.1)
    test(+0.0, +0.0, 5.5)
    test(+0.0, +0.0, -151.189)
    test(+0.0, +0.0, 3.123e-320)
    test(+0.0, +0.0, 2.253547e-318)

    // If n is -0.0, return n
    test(-0.0, -0.0, 2.1)
    test(-0.0, -0.0, 5.5)
    test(-0.0, -0.0, -151.189)
    test(-0.0, -0.0, 3.123e-320)
    test(-0.0, -0.0, 2.253547e-318)

    // Non-special values
    // { val l = List(2.1, 5.5, -151.189, 3.123e-320, 2.253547e-318);
    //   for (n <- l; d <- l) println(s"    test(${n % d}, $n, $d)".toLowerCase()) }
    test(0.0, 2.1, 2.1)
    test(2.1, 2.1, 5.5)
    test(2.1, 2.1, -151.189)
    test(2.0696e-320, 2.1, 3.123e-320)
    test(1.772535e-318, 2.1, 2.253547e-318)
    test(1.2999999999999998, 5.5, 2.1)
    test(0.0, 5.5, 5.5)
    test(5.5, 5.5, -151.189)
    test(3.607e-321, 5.5, 3.123e-320)
    test(6.8103e-319, 5.5, 2.253547e-318)
    test(-2.0889999999999866, -151.189, 2.1)
    test(-2.688999999999993, -151.189, 5.5)
    test(-0.0, -151.189, -151.189)
    test(-1.94e-320, -151.189, 3.123e-320)
    test(-4.1349e-319, -151.189, 2.253547e-318)
    test(3.123e-320, 3.123e-320, 2.1)
    test(3.123e-320, 3.123e-320, 5.5)
    test(3.123e-320, 3.123e-320, -151.189)
    test(0.0, 3.123e-320, 3.123e-320)
    test(3.123e-320, 3.123e-320, 2.253547e-318)
    test(2.253547e-318, 2.253547e-318, 2.1)
    test(2.253547e-318, 2.253547e-318, 5.5)
    test(2.253547e-318, 2.253547e-318, -151.189)
    test(4.995e-321, 2.253547e-318, 3.123e-320)
    test(0.0, 2.253547e-318, 2.253547e-318)
  }

  @Test
  def noReverseComparisons_Issue3575(): Unit = {
    import Double.NaN

    @noinline def test_not_==(x: Double, y: Double): Boolean = !(x == y)
    @noinline def test_not_!=(x: Double, y: Double): Boolean = !(x != y)
    @noinline def test_not_<(x: Double, y: Double): Boolean = !(x < y)
    @noinline def test_not_<=(x: Double, y: Double): Boolean = !(x <= y)
    @noinline def test_not_>(x: Double, y: Double): Boolean = !(x > y)
    @noinline def test_not_>=(x: Double, y: Double): Boolean = !(x >= y)

    assertFalse(test_not_==(5, 5))
    assertTrue(test_not_==(5, 10))
    assertTrue(test_not_==(10, 5))
    assertTrue(test_not_==(5, NaN))
    assertTrue(test_not_==(NaN, NaN))
    assertFalse(test_not_==(0.0, -0.0))

    assertTrue(test_not_!=(5, 5))
    assertFalse(test_not_!=(5, 10))
    assertFalse(test_not_!=(10, 5))
    assertFalse(test_not_!=(5, NaN))
    assertFalse(test_not_!=(NaN, NaN))
    assertTrue(test_not_!=(0.0, -0.0))

    assertTrue(test_not_<(5, 5))
    assertFalse(test_not_<(5, 10))
    assertTrue(test_not_<(10, 5))
    assertTrue(test_not_<(5, NaN))
    assertTrue(test_not_<(NaN, NaN))
    assertTrue(test_not_<(0.0, -0.0))

    assertFalse(test_not_<=(5, 5))
    assertFalse(test_not_<=(5, 10))
    assertTrue(test_not_<=(10, 5))
    assertTrue(test_not_<=(5, NaN))
    assertTrue(test_not_<=(NaN, NaN))
    assertFalse(test_not_<=(0.0, -0.0))

    assertTrue(test_not_>(5, 5))
    assertTrue(test_not_>(5, 10))
    assertFalse(test_not_>(10, 5))
    assertTrue(test_not_>(5, NaN))
    assertTrue(test_not_>(NaN, NaN))
    assertTrue(test_not_>(0.0, -0.0))

    assertFalse(test_not_>=(5, 5))
    assertTrue(test_not_>=(5, 10))
    assertFalse(test_not_>=(10, 5))
    assertTrue(test_not_>=(5, NaN))
    assertTrue(test_not_>=(NaN, NaN))
    assertFalse(test_not_>=(0.0, -0.0))
  }

  @Test
  def negate_Issue4034(): Unit = {
    @noinline
    def testNoInline(expected: Double, x: Double): Unit = {
      assertExactEquals(expected, -x)
      assertExactEquals(expected, -1.0 * x)
    }

    @inline
    def test(expected: Double, x: Double): Unit = {
      testNoInline(expected, x)
      assertExactEquals(expected, -x)
      assertExactEquals(expected, -1.0 * x)
    }

    test(-0.0, 0.0)
    test(0.0, -0.0)
    test(Double.NaN, Double.NaN)
    test(Double.NegativeInfinity, Double.PositiveInfinity)
    test(Double.PositiveInfinity, Double.NegativeInfinity)

    test(-1.5, 1.5)
    test(567.89, -567.89)
  }

  @Test
  def noWrongSimplifications(): Unit = {
    @noinline
    def hide(x: Double): Double = x

    @inline
    def negate(x: Double): Double = -x

    assertExactEquals(0.6000000000000001, (hide(0.1) + 0.2) + 0.3)
    assertExactEquals(0.6, 0.1 + (0.2 + hide(0.3)))

    assertExactEquals(0.0, 0.0 + hide(-0.0))
    assertExactEquals(0.0, 0.0 - hide(0.0))

    assertExactEquals(0.0, negate(negate(hide(0.0))))
    assertExactEquals(-0.0, negate(negate(hide(-0.0))))
  }
}
