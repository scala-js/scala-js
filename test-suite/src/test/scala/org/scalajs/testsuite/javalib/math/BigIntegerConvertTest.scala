/*
 *  Ported by Alistair Johnson from https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerConvertTest.java
 */

package org.scalajs.testsuite.javalib.math

import java.math.BigInteger

import org.scalajs.jasminetest.JasmineTest

object BigIntegerConvertTest extends JasmineTest {

  describe("BigIntegerConvertTest") {

    it("testDoubleValueNegative1") {
      val a = "-27467238945"
      val result = -2.7467238945E10
      val aNumber = new BigInteger(a).doubleValue()
      expect(aNumber).toEqual(result)
    }

    it("testDoubleValueNegative2") {
      val a = "-2746723894572364578265426346273456972"
      val result = -2.7467238945723645E36
      val aNumber = new BigInteger(a).doubleValue()
      expect(aNumber).toEqual(result)
    }

    it("testDoubleValueNegativeInfinity1") {
      val a = "-2746723894572364578265426346273456972283746872364768676747462342342342342342342342323423423423423423426767456345745293762384756238475634563456845634568934568347586346578648576478568456457634875673845678456786587345873645767456834756745763457863485768475678465783456702897830296720476846578634576384567845678346573465786457863"
      val aNumber = new BigInteger(a).doubleValue()
      expect(aNumber).toEqual(Double.NegativeInfinity)
    }

    it("testDoubleValueNegativeInfinity2") {
      val a = Array[Byte](-1, -1, -1, -1, -1, -1, -1, -8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      val aSign = -1
      val aNumber = new BigInteger(aSign, a).doubleValue()
      expect(aNumber).toEqual(Double.NegativeInfinity)
    }

    it("testDoubleValueNegMantissaIsZero") {
      val a = Array[Byte](-128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      val aSign = -1
      val aNumber = new BigInteger(aSign, a).doubleValue()
      expect(aNumber).toEqual(-8.98846567431158E307)
    }

    it("testDoubleValueNegMaxValue") {
      val a = Array[Byte](0, -1, -1, -1, -1, -1, -1, -8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
      val aSign = -1
      val aNumber = new BigInteger(aSign, a).doubleValue()
      expect(aNumber).toEqual(-Double.MaxValue)
    }

    it("testDoubleValueNegNotRounded") {
      val a = Array[Byte](-128, 1, 2, 3, 4, 5, -128, 23, 1, -3, -5)
      val aSign = -1
      val result = -1.5474726438794828E26
      val aNumber = new BigInteger(aSign, a).doubleValue()
      expect(aNumber).toEqual(result)
    }

    it("testDoubleValueNegRounded1") {
      val a = Array[Byte](-128, 1, 2, 3, 4, 5, 60, 23, 1, -3, -5)
      val aSign = -1
      val result = -1.54747264387948E26
      val aNumber = new BigInteger(aSign, a).doubleValue()
      expect(aNumber).toEqual(result)
    }

    it("testDoubleValueNegRounded2") {
      val a = Array[Byte](-128, 1, 2, 3, 4, 5, 36, 23, 1, -3, -5)
      val aSign = -1
      val result = -1.547472643879479E26
      val aNumber = new BigInteger(aSign, a).doubleValue()
      expect(aNumber).toEqual(result)
    }

    it("testDoubleValuePositive1") {
      val a = "27467238945"
      val result = 2.7467238945E10
      val aNumber = new BigInteger(a).doubleValue()
      expect(aNumber).toEqual(result)
    }

    it("testDoubleValuePositive2") {
      val a = "2746723894572364578265426346273456972"
      val result = 2.7467238945723645E36
      val aNumber = new BigInteger(a).doubleValue()
      expect(aNumber).toEqual(result)
    }

    it("testDoubleValuePositiveInfinity1") {
      val a = Array[Byte](-1, -1, -1, -1, -1, -1, -1, -8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      val aSign = 1
      val aNumber = new BigInteger(aSign, a).doubleValue()
      expect(aNumber).toEqual(Double.PositiveInfinity)
    }

    it("testDoubleValuePositiveInfinity2") {
      val a = "2746723894572364578265426346273456972283746872364768676747462342342342342342342342323423423423423423426767456345745293762384756238475634563456845634568934568347586346578648576478568456457634875673845678456786587345873645767456834756745763457863485768475678465783456702897830296720476846578634576384567845678346573465786457863"
      val aNumber = new BigInteger(a).doubleValue()
      expect(aNumber).toEqual(Double.PositiveInfinity)
    }

    it("testDoubleValuePosMantissaIsZero") {
      val a = Array[Byte](-128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      val aSign = 1
      val result = 8.98846567431158E307
      val aNumber = new BigInteger(aSign, a).doubleValue()
      expect(aNumber).toEqual(result)
    }

    it("testDoubleValuePosMaxValue") {
      val a = Array[Byte](0, -1, -1, -1, -1, -1, -1, -8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
      val aSign = 1
      val aNumber = new BigInteger(aSign, a).doubleValue()
      expect(aNumber).toEqual(Double.MaxValue)
    }

    it("testDoubleValuePosNotRounded") {
      val a = Array[Byte](-128, 1, 2, 3, 4, 5, -128, 23, 1, -3, -5)
      val aSign = 1
      val result = 1.5474726438794828E26
      val aNumber = new BigInteger(aSign, a).doubleValue()
      expect(aNumber).toEqual(result)
    }

    it("testDoubleValuePosRounded1") {
      val a = Array[Byte](-128, 1, 2, 3, 4, 5, 60, 23, 1, -3, -5)
      val aSign = 1
      val result = 1.54747264387948E26
      val aNumber = new BigInteger(aSign, a).doubleValue()
      expect(aNumber).toEqual(result)
    }

    it("testDoubleValuePosRounded2") {
      val a = Array[Byte](-128, 1, 2, 3, 4, 5, 36, 23, 1, -3, -5)
      val aSign = 1
      val result = 1.547472643879479E26
      val aNumber = new BigInteger(aSign, a).doubleValue()
      expect(aNumber).toEqual(result)
    }

    it("testDoubleValueZero") {
      val a = "0"
      val result = 0.0
      val aNumber = new BigInteger(a).doubleValue()
      expect(aNumber).toEqual(result)
    }

    // To test that it works with strict floats, do:
    //   > set scalaJSSemantics in testSuite ~= { _.withStrictFloats(true) }
    when("strict-floats").
    it("testFloatValueBug2482") {
      val a = "2147483649"
      val result = 2.14748365E9f
      val aNumber = new BigInteger(a).floatValue()
      val delta = 0
      expect(Math.abs(aNumber - result)).toEqual(delta)
    }

    it("testFloatValueNearNegMaxValue") {
      val a = Array[Byte](0, -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      val aSign = -1
      val aNumber:Float = new BigInteger(aSign, a).floatValue()
      val result = -3.4028235e38
      val delta = 1e31
      expect(Math.abs(aNumber - result)).toBeLessThan(delta)
    }

    it("testFloatValueNearPosMaxValue") {
      val a = Array[Byte](0, -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      val aSign = 1
      val aNumber = new BigInteger(aSign, a).floatValue()
      val result = 3.4028235e38
      val delta = 1e31
      expect(Math.abs(aNumber - result)).toBeLessThan(delta)
    }

    it("testFloatValueNegative1") {
      val a = "-27467238"
      val result = -2.7467238E7f
      val aNumber = new BigInteger(a).floatValue()
      val delta = 1
      expect(Math.abs(aNumber - result)).toBeLessThan(delta)
    }

    it("testFloatValueNegative2") {
      val a = "-27467238945723645782"
      val result = -2.7467239E19f
      val aNumber = new BigInteger(a).floatValue()
      val delta = 1e12
      expect(aNumber - result).toBeLessThan(delta)
    }

    it("testFloatValueNegativeInfinity1") {
      val a = "-2746723894572364578265426346273456972283746872364768676747462342342342342342342342323423423423423423426767456345745293762384756238475634563456845634568934568347586346578648576478568456457634875673845678456786587345873645767456834756745763457863485768475678465783456702897830296720476846578634576384567845678346573465786457863"
      val aNumber = new BigInteger(a).floatValue()
      expect(aNumber).toEqual(Float.NegativeInfinity)
    }

    xit("testFloatValueNegativeInfinity2") {
      val a = Array[Byte](0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
      val aSign = -1
      val aNumber = new BigInteger(aSign, a).floatValue()
      expect(aNumber).toEqual(Float.NegativeInfinity)
    }

    xit("testFloatValueNegMantissaIsZero") {
      val a = Array[Byte](1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      val aSign = -1
      val aNumber = new BigInteger(aSign, a).floatValue()
      expect(aNumber).toEqual(Float.NegativeInfinity)
    }

    it("testFloatValueNegNotRounded") {
      val a = Array[Byte](-128, 1, 2, 3, 4, 5, 60, 23, 1, -3, -5)
      val aSign = -1
      val result = -1.5474726E26f
      val aNumber = new BigInteger(aSign, a).floatValue()
      val delta = 1e19
      expect(aNumber - result).toBeLessThan(delta)
    }

    it("testFloatValueNegRounded1") {
      val a = Array[Byte](-128, 1, -1, -4, 4, 5, 60, 23, 1, -3, -5)
      val aSign = -1
      val result = -1.5475195E26f
      val aNumber = new BigInteger(aSign, a).floatValue()
      val delta = 1e19
      expect(aNumber - result).toBeLessThan(delta)
    }

    it("testFloatValueNegRounded2") {
      val a = Array[Byte](-128, 1, 2, -128, 4, 5, 60, 23, 1, -3, -5)
      val aSign = -1
      val result = -1.5474728E26f
      val aNumber = new BigInteger(aSign, a).floatValue()
      val delta = 1e19
      expect(aNumber - result).toBeLessThan(delta)
    }

    xit("testFloatValuePastNegMaxValue") {
      val a = Array[Byte](0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
      val aSign = -1
      val aNumber = new BigInteger(aSign, a).floatValue()
      expect(aNumber).toEqual(Float.NegativeInfinity)
    }

    xit("testFloatValuePastPosMaxValue") {
      val a = Array[Byte](0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
      val aSign = 1
      val aNumber = new BigInteger(aSign, a).floatValue()
      expect(aNumber).toEqual(Float.PositiveInfinity)
    }

    it("testFloatValuePositive1") {
      val a = "27467238"
      val result = 2.7467238E7f
      val aNumber = new BigInteger(a).floatValue()
      expect(aNumber).toEqual(result)
    }

    it("testFloatValuePositive2") {
      val a = "27467238945723645782"
      val result = 2.7467239E19f
      val aNumber = new BigInteger(a).floatValue()
      val delta = 1e12
      expect(aNumber - result).toBeLessThan(delta)
    }

    xit("testFloatValuePositiveInfinity1") {
      val a = Array[Byte](0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
      val aSign = 1
      val aNumber: Float = new BigInteger(aSign, a).floatValue()
      expect(aNumber).toEqual(Float.PositiveInfinity)
    }

    it("testFloatValuePositiveInfinity2") {
      val a = "274672389457236457826542634627345697228374687236476867674746234" +
        "23423423423423423423234234234234234234267674563457452937623847562384" +
        "75634563456845634568934568347586346578648576478568456457634875673845" +
        "67845678658734587364576745683475674576345786348576847567846578345670" +
        "2897830296720476846578634576384567845678346573465786457863"
      val aNumber = new BigInteger(a).floatValue()
      expect(aNumber).toEqual(Float.PositiveInfinity)
    }

    it("testFloatValuePosMantissaIsZero") {
      val a = Array[Byte](-128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      val aSign = 1
      val result = 1.7014118E38f
      val aNumber = new BigInteger(aSign, a).floatValue()
      val delta = 1e31
      expect(aNumber - result).toBeLessThan(delta)
    }

    it("testFloatValuePosNotRounded") {
      val a = Array[Byte](-128, 1, 2, 3, 4, 5, 60, 23, 1, -3, -5)
      val aSign = 1
      val result = 1.5474726E26f
      val aNumber = new BigInteger(aSign, a).floatValue()
      val delta = 1e19
      expect(aNumber - result).toBeLessThan(delta)
    }

    it("testFloatValuePosRounded1") {
      val a = Array[Byte](-128, 1, -1, -4, 4, 5, 60, 23, 1, -3, -5)
      val aSign = 1
      val result = 1.5475195E26f
      val aNumber = new BigInteger(aSign, a).floatValue()
      val delta = 1e19
      expect(aNumber - result).toBeLessThan(delta)
    }

    it("testFloatValuePosRounded2") {
      val a = Array[Byte](-128, 1, 2, -128, 4, 5, 60, 23, 1, -3, -5)
      val aSign = 1
      val result = 1.5474728E26f
      val aNumber = new BigInteger(aSign, a).floatValue()
      val delta = 1e19
      expect(aNumber - result).toBeLessThan(delta)
    }

    it("testFloatValueZero") {
      val a = "0"
      val result = 0.0f
      val aNumber = new BigInteger(a).floatValue()
      expect(aNumber).toEqual(result)
    }

    it("testIntValueNegative1") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, -128, 45, 91, 3)
      val sign = -1
      val resInt = 2144511229
      val aNumber = new BigInteger(sign, aBytes).intValue()
      expect(aNumber).toEqual(resInt)
    }

    it("testIntValueNegative2") {
      val aBytes = Array[Byte](-12, 56, 100)
      val result = -771996
      val aNumber = new BigInteger(aBytes).intValue()
      expect(aNumber).toEqual(result)
    }

    it("testIntValueNegative3") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 127, 45, 91, 3)
      val sign = -1
      val resInt = -2133678851
      val aNumber = new BigInteger(sign, aBytes).intValue()
      expect(aNumber).toEqual(resInt)
    }

    it("testIntValuePositive1") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3)
      val resInt = 1496144643
      val aNumber = new BigInteger(aBytes).intValue()
      expect(aNumber).toEqual(resInt)
    }

    it("testIntValuePositive2") {
      val aBytes = Array[Byte](12, 56, 100)
      val resInt = 800868
      val aNumber = new BigInteger(aBytes).intValue()
      expect(aNumber).toEqual(resInt)
    }

    it("testIntValuePositive3") {
      val aBytes = Array[Byte](56, 13, 78, -12, -5, 56, 100)
      val sign = 1
      val resInt = -184862620
      val aNumber = new BigInteger(sign, aBytes).intValue()
      expect(aNumber).toEqual(resInt)
    }

    it("testLongValueNegative1") {
      val aBytes = Array[Byte](12, -1, 100, -2, -76, -128, 45, 91, 3)
      val result = -43630045168837885L
      val aNumber = new BigInteger(aBytes).longValue()
      expect(aNumber).toEqual(result)
    }

    it("testLongValueNegative2") {
      val aBytes = Array[Byte](-12, 56, 100, 45, -101, 45, 98)
      val result = -3315696807498398L
      val aNumber = new BigInteger(aBytes).longValue()
      expect(aNumber).toEqual(result)
    }

    it("testLongValuePositive1") {
      val aBytes = Array[Byte](12, 56, 100, -2, -76, 89, 45, 91, 3, 120, -34, -12, 45, 98)
      val result = 3268209772258930018L
      val aNumber = new BigInteger(aBytes).longValue()
      expect(aNumber).toEqual(result)
    }

    it("testLongValuePositive2") {
      val aBytes = Array[Byte](12, 56, 100, 18, -105, 34, -18, 45)
      val result = 880563758158769709L
      val aNumber = new BigInteger(aBytes).longValue()
      expect(aNumber).toEqual(result)
    }

    it("testValueOfIntegerMax") {
      val longVal = Int.MaxValue
      val aNumber = BigInteger.valueOf(longVal)
      val rBytes = Array[Byte](127, -1, -1, -1)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
         expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(1)
    }

    it("testValueOfIntegerMin") {
      val longVal =  Int.MinValue
      val aNumber = BigInteger.valueOf(longVal)
      val rBytes = Array[Byte](-128, 0, 0, 0)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
         expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(-1)
    }

    it("testValueOfLongMax") {
      val longVal = Long.MaxValue
      val aNumber = BigInteger.valueOf(longVal)
      val rBytes = Array[Byte](127, -1, -1, -1, -1, -1, -1, -1)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
         expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(1)
    }

    it("testValueOfLongMin") {
      val longVal = Long.MinValue
      val aNumber = BigInteger.valueOf(longVal)
      val rBytes = Array[Byte](-128, 0, 0, 0, 0, 0, 0, 0)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
         expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(-1)
    }

    it("testValueOfLongNegative1") {
      val longVal = -268209772258930018L
      val aNumber = BigInteger.valueOf(longVal)
      val rBytes = Array[Byte](-4, 71, 32, -94, 23, 55, -46, -98)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
         expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(-1)
    }

    it("testValueOfLongNegative2") {
      val longVal = -58930018L
      val aNumber = BigInteger.valueOf(longVal)
      val rBytes = Array[Byte](-4, 124, -52, -98)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
         expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(-1)
    }

    it("testValueOfLongPositive1") {
      val longVal = 268209772258930018L
      val aNumber = BigInteger.valueOf(longVal)
      val rBytes = Array[Byte](3, -72, -33, 93, -24, -56, 45, 98)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
         expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(1)
    }

    it("testValueOfLongPositive2") {
      val longVal = 58930018L
      val aNumber = BigInteger.valueOf(longVal)
      val rBytes = Array[Byte](3, -125, 51, 98)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
         expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(1)
    }

    it("testValueOfLongZero") {
      val longVal = 0L
      val aNumber = BigInteger.valueOf(longVal)
      val rBytes = Array[Byte](0)
      var resBytes = Array.ofDim[Byte](rBytes.length)
      resBytes = aNumber.toByteArray()
      for (i <- 0 until resBytes.length) {
         expect(resBytes(i)).toEqual(rBytes(i))
      }
      expect(aNumber.signum()).toEqual(0)
    }
  }
}
