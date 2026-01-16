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

package org.scalajs.testsuite.javalib.util.random

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

import java.util.random.RandomGenerator

class RandomGeneratorTest {

  /** Returns a RandomGenerator whose calls to `nextLong()` will return
   *  the elements of `nextLongs`.
   *
   *  It does not loop. Requesting more elements will throw an AssertionError.
   */
  private def make(nextLongs: Long*): RandomGenerator = {
    val iter = nextLongs.iterator
    new RandomGenerator {
      def nextLong(): Long = {
        if (!iter.hasNext)
          throw new AssertionError("Generator has run out")
        iter.next()
      }
    }
  }

  /* 20 random Long values that we use as `nextLong` values in our tests
   * 0xcdff356ba8f073c7L
   * 0x4dd3ce2b9e4f98d3L
   * 0xb85927b9d6e3249bL
   * 0x0bf3df8c2f645e7dL
   * 0x857f63be5f2f9ca0L
   * 0xa12351957679f21fL
   * 0xe01389542264f955L
   * 0x1348eac1e0b1d30dL
   * 0x496620384f075af6L
   * 0xc7a71275b30dde50L
   * 0xff5c1abdc835a88dL
   * 0x7cd709fed6fc0e33L
   * 0x457edabdccfc99b3L
   * 0x867f0bae045cc0b5L
   * 0xd9110b1c6a647a80L
   * 0x27f8c35e876aa500L
   * 0x700455bf2802ddacL
   * 0x06b54104f821ddfeL
   * 0xbf3b87070bbc6313L
   * 0x74fa600cd5741c70L
   */

  @Test def testNextBoolean(): Unit = {
    assertEquals(true, make(0xcdff356ba8f073c7L).nextBoolean())
    assertEquals(false, make(0x4dd3ce2b9e4f98d3L).nextBoolean())
    assertEquals(true, make(0xb85927b9d6e3249bL).nextBoolean())
    assertEquals(false, make(0x0bf3df8c2f645e7dL).nextBoolean())
    assertEquals(true, make(0x857f63be5f2f9ca0L).nextBoolean())
    assertEquals(true, make(0xa12351957679f21fL).nextBoolean())
    assertEquals(true, make(0xe01389542264f955L).nextBoolean())
    assertEquals(false, make(0x1348eac1e0b1d30dL).nextBoolean())
    assertEquals(false, make(0x496620384f075af6L).nextBoolean())
    assertEquals(true, make(0xc7a71275b30dde50L).nextBoolean())
    assertEquals(true, make(0xff5c1abdc835a88dL).nextBoolean())
    assertEquals(false, make(0x7cd709fed6fc0e33L).nextBoolean())
    assertEquals(false, make(0x457edabdccfc99b3L).nextBoolean())
    assertEquals(true, make(0x867f0bae045cc0b5L).nextBoolean())
    assertEquals(true, make(0xd9110b1c6a647a80L).nextBoolean())
    assertEquals(false, make(0x27f8c35e876aa500L).nextBoolean())
    assertEquals(false, make(0x700455bf2802ddacL).nextBoolean())
    assertEquals(false, make(0x06b54104f821ddfeL).nextBoolean())
    assertEquals(true, make(0xbf3b87070bbc6313L).nextBoolean())
    assertEquals(false, make(0x74fa600cd5741c70L).nextBoolean())
  }

  @Test def testNextBytes(): Unit = {
    val a16 = new Array[Byte](16)
    make(0xcdff356ba8f073c7L, 0x4dd3ce2b9e4f98d3L).nextBytes(a16)
    assertArrayEquals(
        Array[Byte](-57, 115, -16, -88, 107, 53, -1, -51, -45, -104, 79, -98, 43, -50, -45, 77),
        a16)

    val a27 = new Array[Byte](27)
    make(0xb85927b9d6e3249bL, 0x0bf3df8c2f645e7dL, 0x857f63be5f2f9ca0L,
        0xa12351957679f21fL).nextBytes(a27)
    assertArrayEquals(
        Array[Byte](-101, 36, -29, -42, -71, 39, 89, -72, 125, 94, 100, 47, -116,
            -33, -13, 11, -96, -100, 47, 95, -66, 99, 127, -123, 31, -14, 121),
        a27)
  }

  @noinline
  private def assertExactEquals(expected: Float, actual: Float): Unit =
    assertTrue(s"expected $expected but got $actual", expected.equals(actual))

  @Test def testNextFloat(): Unit = {
    assertExactEquals(0.8046754f, make(0xcdff356ba8f073c7L).nextFloat())
    assertExactEquals(0.30401313f, make(0x4dd3ce2b9e4f98d3L).nextFloat())
    assertExactEquals(0.72011036f, make(0xb85927b9d6e3249bL).nextFloat())
    assertExactEquals(0.046689928f, make(0x0bf3df8c2f645e7dL).nextFloat())
    assertExactEquals(0.521475f, make(0x857f63be5f2f9ca0L).nextFloat())
    assertExactEquals(0.62944514f, make(0xa12351957679f21fL).nextFloat())
    assertExactEquals(0.8752981f, make(0xe01389542264f955L).nextFloat())
    assertExactEquals(0.07533133f, make(0x1348eac1e0b1d30dL).nextFloat())
    assertExactEquals(0.28671455f, make(0x496620384f075af6L).nextFloat())
    assertExactEquals(0.77989304f, make(0xc7a71275b30dde50L).nextFloat())
    assertExactEquals(0.9974991f, make(0xff5c1abdc835a88dL).nextFloat())
    assertExactEquals(0.48765618f, make(0x7cd709fed6fc0e33L).nextFloat())
    assertExactEquals(0.27146685f, make(0x457edabdccfc99b3L).nextFloat())
    assertExactEquals(0.525376f, make(0x867f0bae045cc0b5L).nextFloat())
    assertExactEquals(0.8479163f, make(0xd9110b1c6a647a80L).nextFloat())
    assertExactEquals(0.15613955f, make(0x27f8c35e876aa500L).nextFloat())
    assertExactEquals(0.4375661f, make(0x700455bf2802ddacL).nextFloat())
    assertExactEquals(0.026203215f, make(0x06b54104f821ddfeL).nextFloat())
    assertExactEquals(0.74700207f, make(0xbf3b87070bbc6313L).nextFloat())
    assertExactEquals(0.45694542f, make(0x74fa600cd5741c70L).nextFloat())
  }

  @Test def testNextFloatWithBound(): Unit = {
    assertExactEquals(0.8046754f, make(0xcdff356ba8f073c7L).nextFloat(1.0f))
    assertExactEquals(0.15200657f, make(0x4dd3ce2b9e4f98d3L).nextFloat(0.5f))
    assertExactEquals(88902.516f, make(0xb85927b9d6e3249bL).nextFloat(123456.789f))
    assertExactEquals(7.0590034f, make(0x0bf3df8c2f645e7dL).nextFloat(151.189f))
    assertExactEquals(0.0f, make(0x857f63be5f2f9ca0L).nextFloat(Float.MinPositiveValue))
    assertExactEquals(2.1418906e38f, make(0xa12351957679f21fL).nextFloat(Float.MaxValue))

    assertThrows(classOf[IllegalArgumentException], make().nextFloat(-10.0f))
    assertThrows(classOf[IllegalArgumentException], make().nextFloat(0.0f))
    assertThrows(classOf[IllegalArgumentException], make().nextFloat(-0.0f))
    assertThrows(classOf[IllegalArgumentException], make().nextFloat(Float.MinValue))
    assertThrows(classOf[IllegalArgumentException], make().nextFloat(Float.PositiveInfinity))
    assertThrows(classOf[IllegalArgumentException], make().nextFloat(Float.NegativeInfinity))
    assertThrows(classOf[IllegalArgumentException], make().nextFloat(Float.NaN))
  }

  @Test def testNextFloatWithOriginAndBound(): Unit = {
    assertExactEquals(0.9023377f, make(0xcdff356ba8f073c7L).nextFloat(0.5f, 1.0f))
    assertExactEquals(-10.63579f, make(0x4dd3ce2b9e4f98d3L).nextFloat(-15.5f, 0.5f))
    assertExactEquals(745774.4f, make(0xb85927b9d6e3249bL).nextFloat(123456.789f, 987654.321f))
    assertExactEquals(-561941.4f, make(0x0bf3df8c2f645e7dL).nextFloat(-589456.0f, -151.189f))
    assertExactEquals(123.456f, make(0x857f63be5f2f9ca0L).nextFloat(123.456f, 123.45601f)) // 1 ulp of difference

    if (!executingInJVMOnLowerThanJDK(19)) {
      // Before JDK 19, these threw IllegalArgumentException's.

      import Float.{MinValue, MaxValue}

      assertExactEquals(
          -5.4696933e37f, make(0xa12351957679f21fL).nextFloat(MinValue, MaxValue / 3.0f))
      assertExactEquals(8.809577e37f, make(0xa12351957679f21fL).nextFloat(MinValue, MaxValue))
      assertExactEquals(1.7215796e38f, make(0xa12351957679f21fL).nextFloat(MinValue / 3.0f, MaxValue))

      assertExactEquals(5.684898e37f, make(0xe01389542264f955L).nextFloat(MinValue, MaxValue / 3.0f))
      assertExactEquals(2.5541462e38f, make(0xe01389542264f955L).nextFloat(MinValue, MaxValue))
      assertExactEquals(2.8370388e38f, make(0xe01389542264f955L).nextFloat(MinValue / 3.0f, MaxValue))

      assertExactEquals(
          -3.0610379e38f, make(0x1348eac1e0b1d30dL).nextFloat(MinValue, MaxValue / 3.0f))
      assertExactEquals(-2.890145e38f, make(0x1348eac1e0b1d30dL).nextFloat(MinValue, MaxValue))
      assertExactEquals(
          -7.9248886e37f, make(0x1348eac1e0b1d30dL).nextFloat(MinValue / 3.0f, MaxValue))
    }

    // Make sure we do not underflow MinPositiveValue to 0.0
    assertExactEquals(Float.MinPositiveValue,
        make(0L).nextFloat(Float.MinPositiveValue, 1024 * Float.MinPositiveValue))

    assertThrows(classOf[IllegalArgumentException], make().nextFloat(-10.0f, -10.0f))
    assertThrows(classOf[IllegalArgumentException], make().nextFloat(-0.0f, 0.0f))
    assertThrows(classOf[IllegalArgumentException], make().nextFloat(5.0f, 4.0f))
    assertThrows(classOf[IllegalArgumentException], make().nextFloat(Float.MinValue, Float.MinValue))
    assertThrows(classOf[IllegalArgumentException], make().nextFloat(Float.NegativeInfinity, -1.0f))
    assertThrows(classOf[IllegalArgumentException], make().nextFloat(1.0f, Float.NegativeInfinity))
    assertThrows(classOf[IllegalArgumentException], make().nextFloat(Float.NaN, 0.0f))
    assertThrows(classOf[IllegalArgumentException], make().nextFloat(0.0f, Float.NaN))
  }

  @noinline
  private def assertExactEquals(expected: Double, actual: Double): Unit =
    assertTrue(s"expected $expected but got $actual", expected.equals(actual))

  @Test def testNextDouble(): Unit = {
    assertExactEquals(0.8046754253236388, make(0xcdff356ba8f073c7L).nextDouble())
    assertExactEquals(0.30401314320471184, make(0x4dd3ce2b9e4f98d3L).nextDouble())
    assertExactEquals(0.7201104000768166, make(0xb85927b9d6e3249bL).nextDouble())
    assertExactEquals(0.04668996021736527, make(0x0bf3df8c2f645e7dL).nextDouble())
    assertExactEquals(0.5214750613951636, make(0x857f63be5f2f9ca0L).nextDouble())
    assertExactEquals(0.6294451703929338, make(0xa12351957679f21fL).nextDouble())
    assertExactEquals(0.8752981024175773, make(0xe01389542264f955L).nextDouble())
    assertExactEquals(0.0753313754400502, make(0x1348eac1e0b1d30dL).nextDouble())
    assertExactEquals(0.28671456694340003, make(0x496620384f075af6L).nextDouble())
    assertExactEquals(0.7798930680610775, make(0xc7a71275b30dde50L).nextDouble())
    assertExactEquals(0.9974991525015954, make(0xff5c1abdc835a88dL).nextDouble())
    assertExactEquals(0.4876562354247512, make(0x7cd709fed6fc0e33L).nextDouble())
    assertExactEquals(0.271466895425862, make(0x457edabdccfc99b3L).nextDouble())
    assertExactEquals(0.5253760623785295, make(0x867f0bae045cc0b5L).nextDouble())
    assertExactEquals(0.8479163116811764, make(0xd9110b1c6a647a80L).nextDouble())
    assertExactEquals(0.1561395746024723, make(0x27f8c35e876aa500L).nextDouble())
    assertExactEquals(0.43756614605809874, make(0x700455bf2802ddacL).nextDouble())
    assertExactEquals(0.026203216279220398, make(0x06b54104f821ddfeL).nextDouble())
    assertExactEquals(0.7470020668222204, make(0xbf3b87070bbc6313L).nextDouble())
    assertExactEquals(0.456945422299626, make(0x74fa600cd5741c70L).nextDouble())
  }

  @Test def testNextDoubleWithBound(): Unit = {
    assertExactEquals(0.8046754253236388, make(0xcdff356ba8f073c7L).nextDouble(1.0))
    assertExactEquals(0.15200657160235592, make(0x4dd3ce2b9e4f98d3L).nextDouble(0.5))
    assertExactEquals(88902.51771898914, make(0xb85927b9d6e3249bL).nextDouble(123456.789))
    assertExactEquals(7.059008395303237, make(0x0bf3df8c2f645e7dL).nextDouble(151.189))
    assertExactEquals(0.0, make(0x857f63be5f2f9ca0L).nextDouble(Double.MinPositiveValue))
    assertExactEquals(1.1315492615876175e308, make(0xa12351957679f21fL).nextDouble(Double.MaxValue))

    assertThrows(classOf[IllegalArgumentException], make().nextDouble(-10.0))
    assertThrows(classOf[IllegalArgumentException], make().nextDouble(0.0))
    assertThrows(classOf[IllegalArgumentException], make().nextDouble(-0.0))
    assertThrows(classOf[IllegalArgumentException], make().nextDouble(Double.MinValue))
    assertThrows(classOf[IllegalArgumentException], make().nextDouble(Double.PositiveInfinity))
    assertThrows(classOf[IllegalArgumentException], make().nextDouble(Double.NegativeInfinity))
    assertThrows(classOf[IllegalArgumentException], make().nextDouble(Double.NaN))
  }

  @Test def testNextDoubleWithOriginAndBound(): Unit = {
    assertExactEquals(0.9023377126618194, make(0xcdff356ba8f073c7L).nextDouble(0.5, 1.0))
    assertExactEquals(-10.63578970872461, make(0x4dd3ce2b9e4f98d3L).nextDouble(-15.5, 0.5))
    assertExactEquals(745774.4195139175, make(0xb85927b9d6e3249bL).nextDouble(123456.789, 987654.321))
    assertExactEquals(-561941.381818508, make(0x0bf3df8c2f645e7dL).nextDouble(-589456.0, -151.189))
    assertExactEquals(123.456, make(0x857f63be5f2f9ca0L).nextDouble(123.456, 123.45600000000002)) // 1 ulp of diff

    if (!executingInJVMOnLowerThanJDK(19)) {
      // Before JDK 19, these threw IllegalArgumentException's.

      import Double.{MinValue, MaxValue}

      assertExactEquals(
          -2.8896078607882544e307, make(0xa12351957679f21fL).nextDouble(MinValue, MaxValue / 3.0))
      assertExactEquals(
          4.654053883129194e307, make(0xa12351957679f21fL).nextDouble(MinValue, MaxValue))
      assertExactEquals(
          9.095013038293849e307, make(0xa12351957679f21fL).nextDouble(MinValue / 3.0, MaxValue))

      assertExactEquals(
          3.003300513698057e307, make(0xe01389542264f955L).nextDouble(MinValue, MaxValue / 3.0))
      assertExactEquals(
          1.349341644485866e308, make(0xe01389542264f955L).nextDouble(MinValue, MaxValue))
      assertExactEquals(
          1.4987921412780162e308, make(0xe01389542264f955L).nextDouble(MinValue / 3.0, MaxValue))

      assertExactEquals(
          -1.6171295395712306e308, make(0x1348eac1e0b1d30dL).nextDouble(MinValue, MaxValue / 3.0))
      assertExactEquals(
          -1.5268477419256879e308, make(0x1348eac1e0b1d30dL).nextDouble(MinValue, MaxValue))
      assertExactEquals(
          -4.1866744966302007e307, make(0x1348eac1e0b1d30dL).nextDouble(MinValue / 3.0, MaxValue))
    }

    // Make sure we do not underflow MinPositiveValue to 0.0
    assertExactEquals(Double.MinPositiveValue,
        make(0L).nextDouble(Double.MinPositiveValue, 1024 * Double.MinPositiveValue))

    assertThrows(classOf[IllegalArgumentException], make().nextDouble(-10.0, -10.0))
    assertThrows(classOf[IllegalArgumentException], make().nextDouble(-0.0, 0.0))
    assertThrows(classOf[IllegalArgumentException], make().nextDouble(5.0, 4.0))
    assertThrows(
        classOf[IllegalArgumentException], make().nextDouble(Double.MinValue, Double.MinValue))
    assertThrows(classOf[IllegalArgumentException], make().nextDouble(Double.NegativeInfinity, -1.0))
    assertThrows(classOf[IllegalArgumentException], make().nextDouble(1.0, Double.NegativeInfinity))
    assertThrows(classOf[IllegalArgumentException], make().nextDouble(Double.NaN, 0.0))
    assertThrows(classOf[IllegalArgumentException], make().nextDouble(0.0, Double.NaN))
  }

  @Test def testNextInt(): Unit = {
    assertEquals(-838912661, make(0xcdff356ba8f073c7L).nextInt())
    assertEquals(1305726507, make(0x4dd3ce2b9e4f98d3L).nextInt())
    assertEquals(-1202116679, make(0xb85927b9d6e3249bL).nextInt())
    assertEquals(200531852, make(0x0bf3df8c2f645e7dL).nextInt())
    assertEquals(-2055248962, make(0x857f63be5f2f9ca0L).nextInt())
    assertEquals(-1591520875, make(0xa12351957679f21fL).nextInt())
    assertEquals(-535590572, make(0xe01389542264f955L).nextInt())
    assertEquals(323545793, make(0x1348eac1e0b1d30dL).nextInt())
    assertEquals(1231429688, make(0x496620384f075af6L).nextInt())
    assertEquals(-945352075, make(0xc7a71275b30dde50L).nextInt())
    assertEquals(-10741059, make(0xff5c1abdc835a88dL).nextInt())
    assertEquals(2094467582, make(0x7cd709fed6fc0e33L).nextInt())
    assertEquals(1165941437, make(0x457edabdccfc99b3L).nextInt())
    assertEquals(-2038494290, make(0x867f0bae045cc0b5L).nextInt())
    assertEquals(-653194468, make(0xd9110b1c6a647a80L).nextInt())
    assertEquals(670614366, make(0x27f8c35e876aa500L).nextInt())
    assertEquals(1879332287, make(0x700455bf2802ddacL).nextInt())
    assertEquals(112541956, make(0x06b54104f821ddfeL).nextInt())
    assertEquals(-1086617849, make(0xbf3b87070bbc6313L).nextInt())
    assertEquals(1962565644, make(0x74fa600cd5741c70L).nextInt())
  }

  @Test def testNextIntWithBound(): Unit = {
    assertEquals(353, make(0xcdff356ba8f073c7L).nextInt(1234))
    assertEquals(652863253, make(0x4dd3ce2b9e4f98d3L).nextInt(987654321))
    assertEquals(1546425308, make(0xb85927b9d6e3249bL).nextInt(2000000000))
    assertEquals(6, make(0x0bf3df8c2f645e7dL).nextInt(10))
    assertEquals(0, make(0x857f63be5f2f9ca0L).nextInt(1))
    assertEquals(1351723210, make(0xa12351957679f21fL).nextInt(Int.MaxValue))

    // Rejects one sample
    assertEquals(1047233791, make(0xff5c1abdc835a88dL, 0x7cd709fed6fc0e33L).nextInt(2000000000))

    // Powers of 2 have a dedicated path
    assertEquals(5, make(0x457edabdccfc99b3L).nextInt(8))
    assertEquals(942, make(0x867f0bae045cc0b5L).nextInt(1024))
    assertEquals(420547356, make(0xd9110b1c6a647a80L).nextInt(1 << 30))

    assertThrows(classOf[IllegalArgumentException], make().nextInt(0))
    assertThrows(classOf[IllegalArgumentException], make().nextInt(-10))
    assertThrows(classOf[IllegalArgumentException], make().nextInt(Int.MinValue))
  }

  @Test def testNextIntWithOriginAndBound(): Unit = {
    assertEquals(3353, make(0xcdff356ba8f073c7L).nextInt(3000, 4234))
    assertEquals(529406464, make(0x4dd3ce2b9e4f98d3L).nextInt(-123456789, 987654321))
    assertEquals(-453574692, make(0xb85927b9d6e3249bL).nextInt(-2000000000, -123456789))
    assertEquals(-4, make(0x0bf3df8c2f645e7dL).nextInt(-10, 0))

    assertEquals(56, make(0x857f63be5f2f9ca0L).nextInt(56, 57)) // no choice

    // Close to min or max
    assertEquals(-2147483647, make(0xa12351957679f21fL).nextInt(Int.MinValue, Int.MinValue + 3))
    assertEquals(2147483645, make(0xe01389542264f955L).nextInt(Int.MaxValue - 3, Int.MaxValue))
    assertEquals(161772897, make(0x1348eac1e0b1d30dL).nextInt(1, Int.MaxValue))

    // Rejects one sample
    assertEquals(
        47233791, make(0xff5c1abdc835a88dL, 0x7cd709fed6fc0e33L).nextInt(-1000000000, 1000000000))

    // bound - origin overflows
    assertEquals(1165941437, make(0x457edabdccfc99b3L).nextInt(-1000000000, 2000000000))

    // bound - origin overflows and rejects one sample
    assertEquals(
        112541956, make(0x77fa600cd5741c70L, 0x06b54104f821ddfeL).nextInt(-1000000000, 2000000000))

    // Powers of 2 have a dedicated path
    assertEquals(45, make(0x457edabdccfc99b3L).nextInt(40, 40 + 8))
    assertEquals(-999999058, make(0x867f0bae045cc0b5L).nextInt(-1000000000, -1000000000 + 1024))
    assertEquals(420090567, make(0xd9110b1c6a647a80L).nextInt(-456789, -456789 + (1 << 30)))

    // The power of 2 that also overflows
    assertEquals(670157577, make(0x27f8c35e876aa500L).nextInt(-456789, -456789 + (1 << 31)))

    assertThrows(classOf[IllegalArgumentException], make().nextInt(56, 56))
    assertThrows(classOf[IllegalArgumentException], make().nextInt(-10, -30))
    assertThrows(classOf[IllegalArgumentException], make().nextInt(Int.MaxValue, Int.MinValue))
  }

  @Test def testNextLong(): Unit = {
    assertEquals(-3603102440361004089L, make(0xcdff356ba8f073c7L).nextLong())
    assertEquals(5608052647741331667L, make(0x4dd3ce2b9e4f98d3L).nextLong())
    assertEquals(-5163051818675919717L, make(0xb85927b9d6e3249bL).nextLong())
    assertEquals(861277746941419133L, make(0x0bf3df8c2f645e7dL).nextLong())
    assertEquals(-8827227075330990944L, make(0x857f63be5f2f9ca0L).nextLong())
    assertEquals(-6835530107038600673L, make(0xa12351957679f21fL).nextLong())
    assertEquals(-2300343990208890539L, make(0xe01389542264f955L).nextLong())
    assertEquals(1389618603463136013L, make(0x1348eac1e0b1d30dL).nextLong())
    assertEquals(5288950238609365750L, make(0x496620384f075af6L).nextLong())
    assertEquals(-4060256242326708656L, make(0xc7a71275b30dde50L).nextLong())
    assertEquals(-46132493770446707L, make(0xff5c1abdc835a88dL).nextLong())
    assertEquals(8995669770829041203L, make(0x7cd709fed6fc0e33L).nextLong())
    assertEquals(5007680344405350835L, make(0x457edabdccfc99b3L).nextLong())
    assertEquals(-8755266308559552331L, make(0x867f0bae045cc0b5L).nextLong())
    assertEquals(-2805448876203148672L, make(0xd9110b1c6a647a80L).nextLong())
    assertEquals(2880266772469687552L, make(0x27f8c35e876aa500L).nextLong())
    assertEquals(8071670711653162412L, make(0x700455bf2802ddacL).nextLong())
    assertEquals(483364024610840062L, make(0x06b54104f821ddfeL).nextLong())
    assertEquals(-4666988124507970797L, make(0xbf3b87070bbc6313L).nextLong())
    assertEquals(8429155260814335088L, make(0x74fa600cd5741c70L).nextLong())
  }

  @Test def testNextLongWithBound(): Unit = {
    assertEquals(1179L, make(0xcdff356ba8f073c7L).nextLong(1234L))
    assertEquals(6567945330301L, make(0x4dd3ce2b9e4f98d3L).nextLong(9876543219876L))
    assertEquals(2641846127516815949L, make(0xb85927b9d6e3249bL).nextLong(4000000000000000000L))
    assertEquals(6, make(0x0bf3df8c2f645e7dL).nextLong(10L))
    assertEquals(0, make(0x857f63be5f2f9ca0L).nextLong(1L))
    assertEquals(5805606983335475471L, make(0xa12351957679f21fL).nextLong(Long.MaxValue))

    // Rejects one sample
    assertEquals(497834885414520601L,
        make(0xff5c1abdc835a88dL, 0x7cd709fed6fc0e33L).nextLong(4000000000000000000L))

    // Powers of 2 have a dedicated path
    assertEquals(3L, make(0x457edabdccfc99b3L).nextLong(8L))
    assertEquals(181L, make(0x867f0bae045cc0b5L).nextLong(1024L))
    assertEquals(711228032L, make(0xd9110b1c6a647a80L).nextLong(1L << 30))
    assertEquals(214810766255360L, make(0x27f8c35e876aa500L).nextLong(1L << 50))
    assertEquals(3459984693225774508L, make(0x700455bf2802ddacL).nextLong(1L << 62))

    assertThrows(classOf[IllegalArgumentException], make().nextLong(0L))
    assertThrows(classOf[IllegalArgumentException], make().nextLong(-10L))
    assertThrows(classOf[IllegalArgumentException], make().nextLong(Long.MinValue))
  }

  @Test def testNextLongWithOriginAndBound(): Unit = {
    assertEquals(4179L, make(0xcdff356ba8f073c7L).nextLong(3000L, 4234L))
    assertEquals(2867080832779L, make(0x4dd3ce2b9e4f98d3L).nextLong(-1234567891234L, 9876543219876L))
    assertEquals(-1358152637915292817L,
        make(0xb85927b9d6e3249bL).nextLong(-4000000000000000000L, -1234567891234L))
    assertEquals(-4L, make(0x0bf3df8c2f645e7dL).nextLong(-10L, 0L))

    assertEquals(56L, make(0x857f63be5f2f9ca0L).nextLong(56L, 57L)) // no choice

    // Close to min or max
    assertEquals(
        -9223372036854775806L, make(0xa12351957679f21fL).nextLong(Long.MinValue, Long.MinValue + 3L))
    assertEquals(
        9223372036854775806L, make(0xe01389542264f955L).nextLong(Long.MaxValue - 3L, Long.MaxValue))
    assertEquals(694809301731568007L, make(0x1348eac1e0b1d30dL).nextLong(1, Long.MaxValue))

    // Rejects one sample
    assertEquals(-1502165114585479399L,
        make(0xff5c1abdc835a88dL, 0x7cd709fed6fc0e33L).nextLong(
            -2000000000000000000L, 2000000000000000000L))

    // bound - origin overflows
    assertEquals(861277746941419133L,
        make(0x0bf3df8c2f645e7dL).nextLong(-6000000000000000000L, 4000000000000000000L))

    // bound - origin overflows and rejects one sample
    assertEquals(483364024610840062L,
        make(0x457edabdccfc99b3L, 0x06b54104f821ddfeL).nextLong(
            -6000000000000000000L, 4000000000000000000L))

    // Powers of 2 have a dedicated path
    assertEquals(43L, make(0x457edabdccfc99b3L).nextLong(40L, 40L + 8L))
    assertEquals(-999999819L, make(0x867f0bae045cc0b5L).nextLong(-1000000000L, -1000000000 + 1024L))
    assertEquals(710771243L, make(0xd9110b1c6a647a80L).nextLong(-456789L, -456789L + (1L << 30)))
    assertEquals(2000214810766255360L,
        make(0x27f8c35e876aa500L).nextLong(2000000000000000000L, 2000000000000000000L + (1L << 50)))
    assertEquals(3459983458657883274L,
        make(0x700455bf2802ddacL).nextLong(-1234567891234L, -1234567891234L + (1L << 62)))

    // The power of 2 that also overflows
    assertEquals(4556382677778913777L,
        make(0xbf3b87070bbc6313L).nextLong(-1234567891234L, -1234567891234L + (1L << 63)))

    assertThrows(classOf[IllegalArgumentException], make().nextLong(56L, 56L))
    assertThrows(classOf[IllegalArgumentException], make().nextLong(-10L, -30L))
    assertThrows(classOf[IllegalArgumentException], make().nextLong(Long.MaxValue, Long.MinValue))
  }
}
