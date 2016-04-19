// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerConvertTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math.BigInteger

import org.junit.{Test, Ignore}
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform._

class BigIntegerConvertTest {

  @Test def testDoubleValueNegative1(): Unit = {
    val a = "-27467238945"
    val result = -2.7467238945E10
    val aNumber = new BigInteger(a).doubleValue()
    assertEquals(result, aNumber, 0.0)
  }

  @Test def testDoubleValueNegative2(): Unit = {
    val a = "-2746723894572364578265426346273456972"
    val result = -2.7467238945723645E36
    val aNumber = new BigInteger(a).doubleValue()
    assertEquals(result, aNumber, 0.0)
  }

  @Test def testDoubleValueNegativeInfinity1(): Unit = {
    val a = "-274672389457236457826542634627345697228374687236476867674746" +
      "2342342342342342342342323423423423423423426767456345745293762384756" +
      "2384756345634568456345689345683475863465786485764785684564576348756" +
      "7384567845678658734587364576745683475674576345786348576847567846578" +
      "3456702897830296720476846578634576384567845678346573465786457863"
    val aNumber = new BigInteger(a).doubleValue()
    assertEquals(Double.NegativeInfinity, aNumber, 0.0)
  }

  @Test def testDoubleValueNegativeInfinity2(): Unit = {
    val a = Array[Byte](-1, -1, -1, -1, -1, -1, -1, -8, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val aSign = -1
    val aNumber = new BigInteger(aSign, a).doubleValue()
    assertEquals(Double.NegativeInfinity, aNumber, 0.0)
  }

  @Test def testDoubleValueNegMantissaIsZero(): Unit = {
    val a = Array[Byte](-128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val aSign = -1
    val aNumber = new BigInteger(aSign, a).doubleValue()
    assertEquals(-8.98846567431158E307, aNumber, 0.0)
  }

  @Test def testDoubleValueNegMaxValue(): Unit = {
    val a = Array[Byte](0, -1, -1, -1, -1, -1, -1, -8, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
    val aSign = -1
    val aNumber = new BigInteger(aSign, a).doubleValue()
    assertEquals(-Double.MaxValue, aNumber, 0.0)
  }

  @Test def testDoubleValueNegNotRounded(): Unit = {
    val a = Array[Byte](-128, 1, 2, 3, 4, 5, -128, 23, 1, -3, -5)
    val aSign = -1
    val result = -1.5474726438794828E26
    val aNumber = new BigInteger(aSign, a).doubleValue()
    assertEquals(result, aNumber, 0.0)
  }

  @Test def testDoubleValueNegRounded1(): Unit = {
    val a = Array[Byte](-128, 1, 2, 3, 4, 5, 60, 23, 1, -3, -5)
    val aSign = -1
    val result = -1.54747264387948E26
    val aNumber = new BigInteger(aSign, a).doubleValue()
    assertEquals(result, aNumber, 0.0)
  }

  @Test def testDoubleValueNegRounded2(): Unit = {
    val a = Array[Byte](-128, 1, 2, 3, 4, 5, 36, 23, 1, -3, -5)
    val aSign = -1
    val result = -1.547472643879479E26
    val aNumber = new BigInteger(aSign, a).doubleValue()
    assertEquals(result, aNumber, 0.0)
  }

  @Test def testDoubleValuePositive1(): Unit = {
    val a = "27467238945"
    val result = 2.7467238945E10
    val aNumber = new BigInteger(a).doubleValue()
    assertEquals(result, aNumber, 0.0)
  }

  @Test def testDoubleValuePositive2(): Unit = {
    val a = "2746723894572364578265426346273456972"
    val result = 2.7467238945723645E36
    val aNumber = new BigInteger(a).doubleValue()
    assertEquals(result, aNumber, 0.0)
  }

  @Test def testDoubleValuePositiveInfinity1(): Unit = {
    val a = Array[Byte](-1, -1, -1, -1, -1, -1, -1, -8, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val aSign = 1
    val aNumber = new BigInteger(aSign, a).doubleValue()
    assertEquals(Double.PositiveInfinity, aNumber, 0.0)
  }

  @Test def testDoubleValuePositiveInfinity2(): Unit = {
    val a = "2746723894572364578265426346273456972283746872364768676747462" +
      "3423423423423423423423234234234234234234267674563457452937623847562" +
      "3847563456345684563456893456834758634657864857647856845645763487567" +
      "3845678456786587345873645767456834756745763457863485768475678465783" +
      "456702897830296720476846578634576384567845678346573465786457863"
    val aNumber = new BigInteger(a).doubleValue()
    assertEquals(Double.PositiveInfinity, aNumber, 0.0)
  }

  @Test def testDoubleValuePosMantissaIsZero(): Unit = {
    val a = Array[Byte](-128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val aSign = 1
    val result = 8.98846567431158E307
    val aNumber = new BigInteger(aSign, a).doubleValue()
    assertEquals(result, aNumber, 0.0)
  }

  @Test def testDoubleValuePosMaxValue(): Unit = {
    val a = Array[Byte](0, -1, -1, -1, -1, -1, -1, -8, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
    val aSign = 1
    val aNumber = new BigInteger(aSign, a).doubleValue()
    assertEquals(Double.MaxValue, aNumber, 0.0)
  }

  @Test def testDoubleValuePosNotRounded(): Unit = {
    val a = Array[Byte](-128, 1, 2, 3, 4, 5, -128, 23, 1, -3, -5)
    val aSign = 1
    val result = 1.5474726438794828E26
    val aNumber = new BigInteger(aSign, a).doubleValue()
    assertEquals(result, aNumber, 0.0)
  }

  @Test def testDoubleValuePosRounded1(): Unit = {
    val a = Array[Byte](-128, 1, 2, 3, 4, 5, 60, 23, 1, -3, -5)
    val aSign = 1
    val result = 1.54747264387948E26
    val aNumber = new BigInteger(aSign, a).doubleValue()
    assertEquals(result, aNumber, 0.0)
  }

  @Test def testDoubleValuePosRounded2(): Unit = {
    val a = Array[Byte](-128, 1, 2, 3, 4, 5, 36, 23, 1, -3, -5)
    val aSign = 1
    val result = 1.547472643879479E26
    val aNumber = new BigInteger(aSign, a).doubleValue()
    assertEquals(result, aNumber, 0.0)
  }

  @Test def testDoubleValueZero(): Unit = {
    val a = "0"
    val result = 0.0
    val aNumber = new BigInteger(a).doubleValue()
    assertEquals(result, aNumber, 0.0)
  }

  @Test def testFloatValueNearNegMaxValue(): Unit = {
    val a = Array[Byte](0, -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val aSign = -1
    val aNumber:Float = new BigInteger(aSign, a).floatValue()
    val result = -3.4028235e38
    val delta = 1e31
    assertTrue(Math.abs(aNumber - result) < delta)
  }

  @Test def testFloatValueNearPosMaxValue(): Unit = {
    val a = Array[Byte](0, -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val aSign = 1
    val aNumber = new BigInteger(aSign, a).floatValue()
    val result = 3.4028235e38
    val delta = 1e31
    assertTrue(Math.abs(aNumber - result) < delta)
  }

  @Test def testFloatValueNegative1(): Unit = {
    val a = "-27467238"
    val result = -2.7467238E7f
    val aNumber = new BigInteger(a).floatValue()
    val delta = 1
    assertTrue(Math.abs(aNumber - result) < delta)
  }

  @Test def testFloatValueNegative2(): Unit = {
    val a = "-27467238945723645782"
    val result = -2.7467239E19f
    val aNumber = new BigInteger(a).floatValue()
    val delta = 1e12
    assertTrue(aNumber - result < delta)
  }

  @Test def testFloatValueNegativeInfinity1(): Unit = {
    val a = "-274672389457236457826542634627345697228374687236476867674746" +
      "2342342342342342342342323423423423423423426767456345745293762384756" +
      "2384756345634568456345689345683475863465786485764785684564576348756" +
      "7384567845678658734587364576745683475674576345786348576847567846578" +
      "3456702897830296720476846578634576384567845678346573465786457863"
    val aNumber = new BigInteger(a).floatValue()
    assertEquals(Float.NegativeInfinity, aNumber, 0.0f)
  }

  @Ignore @Test def testFloatValueNegativeInfinity2(): Unit = {
    val a = Array[Byte](0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
    val aSign = -1
    val aNumber = new BigInteger(aSign, a).floatValue()
    assertEquals(Float.NegativeInfinity, aNumber, 0.0f)
  }

  @Ignore @Test def testFloatValueNegMantissaIsZero(): Unit = {
    val a = Array[Byte](1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val aSign = -1
    val aNumber = new BigInteger(aSign, a).floatValue()
    assertEquals(Float.NegativeInfinity, aNumber, 0.0f)
  }

  @Test def testFloatValueNegNotRounded(): Unit = {
    val a = Array[Byte](-128, 1, 2, 3, 4, 5, 60, 23, 1, -3, -5)
    val aSign = -1
    val result = -1.5474726E26f
    val aNumber = new BigInteger(aSign, a).floatValue()
    val delta = 1e19
    assertTrue(aNumber - result < delta)
  }

  @Test def testFloatValueNegRounded1(): Unit = {
    val a = Array[Byte](-128, 1, -1, -4, 4, 5, 60, 23, 1, -3, -5)
    val aSign = -1
    val result = -1.5475195E26f
    val aNumber = new BigInteger(aSign, a).floatValue()
    val delta = 1e19
    assertTrue(aNumber - result < delta)
  }

  @Test def testFloatValueNegRounded2(): Unit = {
    val a = Array[Byte](-128, 1, 2, -128, 4, 5, 60, 23, 1, -3, -5)
    val aSign = -1
    val result = -1.5474728E26f
    val aNumber = new BigInteger(aSign, a).floatValue()
    val delta = 1e19
    assertTrue(aNumber - result < delta)
  }

  @Ignore @Test def testFloatValuePastNegMaxValue(): Unit = {
    val a = Array[Byte](0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
    val aSign = -1
    val aNumber = new BigInteger(aSign, a).floatValue()
    assertEquals(Float.NegativeInfinity, aNumber, 0.0f)
  }

  @Ignore @Test def testFloatValuePastPosMaxValue(): Unit = {
    val a = Array[Byte](0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
    val aSign = 1
    val aNumber = new BigInteger(aSign, a).floatValue()
    assertEquals(Float.PositiveInfinity, aNumber, 0.0f)
  }

  @Test def testFloatValuePositive1(): Unit = {
    val a = "27467238"
    val result = 2.7467238E7f
    val aNumber = new BigInteger(a).floatValue()
    assertEquals(result, aNumber, 0.0f)
  }

  @Test def testFloatValuePositive2(): Unit = {
    val a = "27467238945723645782"
    val result = 2.7467239E19f
    val aNumber = new BigInteger(a).floatValue()
    val delta = 1e12
    assertTrue(aNumber - result < delta)
  }

  @Ignore @Test def testFloatValuePositiveInfinity1(): Unit = {
    val a = Array[Byte](0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
    val aSign = 1
    val aNumber: Float = new BigInteger(aSign, a).floatValue()
    assertEquals(Float.PositiveInfinity, aNumber, 0.0f)
  }

  @Test def testFloatValuePositiveInfinity2(): Unit = {
    val a = "274672389457236457826542634627345697228374687236476867674746234" +
      "23423423423423423423234234234234234234267674563457452937623847562384" +
      "75634563456845634568934568347586346578648576478568456457634875673845" +
      "67845678658734587364576745683475674576345786348576847567846578345670" +
      "2897830296720476846578634576384567845678346573465786457863"
    val aNumber = new BigInteger(a).floatValue()
    assertEquals(Float.PositiveInfinity, aNumber, 0.0f)
  }

  @Test def testFloatValuePosMantissaIsZero(): Unit = {
    val a = Array[Byte](-128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val aSign = 1
    val result = 1.7014118E38f
    val aNumber = new BigInteger(aSign, a).floatValue()
    val delta = 1e31
    assertTrue(aNumber - result < delta)
  }

  @Test def testFloatValuePosNotRounded(): Unit = {
    val a = Array[Byte](-128, 1, 2, 3, 4, 5, 60, 23, 1, -3, -5)
    val aSign = 1
    val result = 1.5474726E26f
    val aNumber = new BigInteger(aSign, a).floatValue()
    val delta = 1e19
    assertTrue(aNumber - result < delta)
  }

  @Test def testFloatValuePosRounded1(): Unit = {
    val a = Array[Byte](-128, 1, -1, -4, 4, 5, 60, 23, 1, -3, -5)
    val aSign = 1
    val result = 1.5475195E26f
    val aNumber = new BigInteger(aSign, a).floatValue()
    val delta = 1e19
    assertTrue(aNumber - result < delta)
  }

  @Test def testFloatValuePosRounded2(): Unit = {
    val a = Array[Byte](-128, 1, 2, -128, 4, 5, 60, 23, 1, -3, -5)
    val aSign = 1
    val result = 1.5474728E26f
    val aNumber = new BigInteger(aSign, a).floatValue()
    val delta = 1e19
    assertTrue(aNumber - result < delta)
  }

  @Test def testFloatValueZero(): Unit = {
    val a = "0"
    val result = 0.0f
    val aNumber = new BigInteger(a).floatValue()
    assertEquals(result, aNumber, 0.0f)
  }

  @Test def testIntValueNegative1(): Unit = {
    val aBytes = Array[Byte](12, 56, 100, -2, -76, -128, 45, 91, 3)
    val sign = -1
    val resInt = 2144511229
    val aNumber = new BigInteger(sign, aBytes).intValue()
    assertEquals(resInt, aNumber)
  }

  @Test def testIntValueNegative2(): Unit = {
    val aBytes = Array[Byte](-12, 56, 100)
    val result = -771996
    val aNumber = new BigInteger(aBytes).intValue()
    assertEquals(result, aNumber)
  }

  @Test def testIntValueNegative3(): Unit = {
    val aBytes = Array[Byte](12, 56, 100, -2, -76, 127, 45, 91, 3)
    val sign = -1
    val resInt = -2133678851
    val aNumber = new BigInteger(sign, aBytes).intValue()
    assertEquals(resInt, aNumber)
  }

  @Test def testIntValuePositive1(): Unit = {
    val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3)
    val resInt = 1496144643
    val aNumber = new BigInteger(aBytes).intValue()
    assertEquals(resInt, aNumber)
  }

  @Test def testIntValuePositive2(): Unit = {
    val aBytes = Array[Byte](12, 56, 100)
    val resInt = 800868
    val aNumber = new BigInteger(aBytes).intValue()
    assertEquals(resInt, aNumber)
  }

  @Test def testIntValuePositive3(): Unit = {
    val aBytes = Array[Byte](56, 13, 78, -12, -5, 56, 100)
    val sign = 1
    val resInt = -184862620
    val aNumber = new BigInteger(sign, aBytes).intValue()
    assertEquals(resInt, aNumber)
  }

  @Test def testLongValueNegative1(): Unit = {
    val aBytes = Array[Byte](12, -1, 100, -2, -76, -128, 45, 91, 3)
    val result = -43630045168837885L
    val aNumber = new BigInteger(aBytes).longValue()
    assertEquals(result, aNumber)
  }

  @Test def testLongValueNegative2(): Unit = {
    val aBytes = Array[Byte](-12, 56, 100, 45, -101, 45, 98)
    val result = -3315696807498398L
    val aNumber = new BigInteger(aBytes).longValue()
    assertEquals(result, aNumber)
  }

  @Test def testLongValuePositive1(): Unit = {
    val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, 120, -34, -12, 45, 98)
    val result = 3268209772258930018L
    val aNumber = new BigInteger(aBytes).longValue()
    assertEquals(result, aNumber)
  }

  @Test def testLongValuePositive2(): Unit = {
    val aBytes = Array[Byte](12, 56, 100, 18, -105, 34, -18, 45)
    val result = 880563758158769709L
    val aNumber = new BigInteger(aBytes).longValue()
    assertEquals(result, aNumber)
  }

  @Test def testValueOfIntegerMax(): Unit = {
    val longVal = Int.MaxValue
    val aNumber = BigInteger.valueOf(longVal)
    val rBytes = Array[Byte](127, -1, -1, -1)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
       assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, aNumber.signum())
  }

  @Test def testValueOfIntegerMin(): Unit = {
    val longVal =  Int.MinValue
    val aNumber = BigInteger.valueOf(longVal)
    val rBytes = Array[Byte](-128, 0, 0, 0)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
       assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, aNumber.signum())
  }

  @Test def testValueOfLongMax(): Unit = {
    val longVal = Long.MaxValue
    val aNumber = BigInteger.valueOf(longVal)
    val rBytes = Array[Byte](127, -1, -1, -1, -1, -1, -1, -1)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
       assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, aNumber.signum())
  }

  @Test def testValueOfLongMin(): Unit = {
    val longVal = Long.MinValue
    val aNumber = BigInteger.valueOf(longVal)
    val rBytes = Array[Byte](-128, 0, 0, 0, 0, 0, 0, 0)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
       assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, aNumber.signum())
  }

  @Test def testValueOfLongNegative1(): Unit = {
    val longVal = -268209772258930018L
    val aNumber = BigInteger.valueOf(longVal)
    val rBytes = Array[Byte](-4, 71, 32, -94, 23, 55, -46, -98)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
       assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, aNumber.signum())
  }

  @Test def testValueOfLongNegative2(): Unit = {
    val longVal = -58930018L
    val aNumber = BigInteger.valueOf(longVal)
    val rBytes = Array[Byte](-4, 124, -52, -98)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
       assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(-1, aNumber.signum())
  }

  @Test def testValueOfLongPositive1(): Unit = {
    val longVal = 268209772258930018L
    val aNumber = BigInteger.valueOf(longVal)
    val rBytes = Array[Byte](3, -72, -33, 93, -24, -56, 45, 98)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
       assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, aNumber.signum())
  }

  @Test def testValueOfLongPositive2(): Unit = {
    val longVal = 58930018L
    val aNumber = BigInteger.valueOf(longVal)
    val rBytes = Array[Byte](3, -125, 51, 98)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
       assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(1, aNumber.signum())
  }

  @Test def testValueOfLongZero(): Unit = {
    val longVal = 0L
    val aNumber = BigInteger.valueOf(longVal)
    val rBytes = Array[Byte](0)
    var resBytes = Array.ofDim[Byte](rBytes.length)
    resBytes = aNumber.toByteArray()
    for (i <- 0 until resBytes.length) {
       assertEquals(rBytes(i), resBytes(i))
    }
    assertEquals(0, aNumber.signum())
  }

  @Test def testFloatValueBug2482(): Unit = {
    // To test that it works with strict floats, do:
    //   > set scalaJSSemantics in testSuite ~= { _.withStrictFloats(true) }
    assumeTrue("Assumed strict floats", hasStrictFloats)

    val a = "2147483649"
    val result = 2.14748365E9f
    val aNumber = new BigInteger(a).floatValue()
    val delta = 0.0f
    assertEquals(delta, Math.abs(aNumber - result), 0.0f)
  }
}
