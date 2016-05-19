// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigDecimalConvertTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math._

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

class BigDecimalConvertTest {

  @Test def testByteValue(): Unit = {
    assertEquals(1.toByte, BigDecimal.ONE.byteValue())
    assertEquals(BigDecimal.valueOf(255).byteValue(), -1.toByte)
    assertEquals(BigDecimal.ONE.byteValueExact(), 1.toByte)
    expectThrows(classOf[ArithmeticException], BigDecimal.valueOf(255).byteValueExact())
  }

  @Test def testDoubleValueNeg(): Unit = {
    val a = "-123809648392384754573567356745735.63567890295784902768787678287E+21"
    val aNumber = new BigDecimal(a)
    val result = -1.2380964839238476E53
    assertEquals(aNumber.doubleValue(), result, 0.0)
  }

  @Test def testDoubleValueNegInfinity(): Unit = {
    val a = "-123809648392384754573567356745735.63567890295784902768787678287E+400"
    val aNumber = new BigDecimal(a)
    val result = Double.NegativeInfinity
    assertTrue(result == aNumber.doubleValue())
  }

  @Test def testDoubleValuePos(): Unit = {
    val a = "123809648392384754573567356745735.63567890295784902768787678287E+21"
    val aNumber = new BigDecimal(a)
    val result = 1.2380964839238476E53
    assertEquals(aNumber.doubleValue(), result, 0.0)
  }

  @Test def testDoubleValuePosInfinity(): Unit = {
    val a = "123809648392384754573567356745735.63567890295784902768787678287E+400"
    val aNumber = new BigDecimal(a)
    val result = Double.PositiveInfinity
    assertTrue(result == aNumber.doubleValue())
  }

  @Test def testFloatValueNeg(): Unit = {
    val a = "-1238096483923847.6356789029578E+21"
    val aNumber = new BigDecimal(a)
    val result = -1.2380965E36f
    assertTrue(Math.abs(aNumber.floatValue() - result) < 1E29)
  }

  @Test def testFloatValueNegInfinity(): Unit = {
    val a = "-123809648392384755735.63567887678287E+200"
    val aNumber = new BigDecimal(a)
    val result =  Float.NegativeInfinity
    assertTrue(aNumber.floatValue() == result)
  }

  @Test def testFloatValuePos(): Unit = {
    val a = "1238096483923847.6356789029578E+21"
    val aNumber = new BigDecimal(a)
    val result = 1.2380965E36f
    assertTrue(Math.abs(aNumber.floatValue() - result) < 1E29)
  }

  @Test def testFloatValuePosInfinity(): Unit = {
    val a = "123809648373567356745735.6356789787678287E+200"
    val aNumber = new BigDecimal(a)
    val result =  Float.PositiveInfinity
    assertTrue(aNumber.floatValue() == result)
  }

  @Test def testIntValueNeg(): Unit = {
    val a = "-123809648392384754573567356745735.63567890295784902768787678287E+21"
    val aNumber = new BigDecimal(a)
    val result = 218520473
    assertEquals(aNumber.intValue(), result)
    expectThrows(classOf[ArithmeticException], aNumber.intValueExact())
  }

  @Test def testIntValuePos(): Unit = {
    val a = "123809648392384754573567356745735.63567890295784902768787678287E+21"
    val aNumber = new BigDecimal(a)
    val result = -218520473
    assertEquals(aNumber.intValue(), result)
    expectThrows(classOf[ArithmeticException], aNumber.intValueExact())
  }

  @Test def testLongValueNeg(): Unit = {
    val a = "-123809648392384754573567356745735.63567890295784902768787678287E+21"
    val aNumber = new BigDecimal(a)
    val result = -1246043477766677607L
    assertTrue(aNumber.longValue() == result)
    expectThrows(classOf[ArithmeticException], aNumber.longValueExact())
  }

  @Test def testLongValuePos(): Unit = {
    val a = "123809648392384754573567356745735.63567890295784902768787678287E+21"
    val aNumber = new BigDecimal(a)
    val result = 1246043477766677607L
    assertTrue(aNumber.longValue() == result)
    expectThrows(classOf[ArithmeticException], aNumber.longValueExact())
  }

  @Test def testLongValueMinMaxValues(): Unit = {
    val longMaxValue = new BigDecimal(Long.MaxValue)
    val longMinValue = new BigDecimal(Long.MinValue)

    assertEquals(Long.MaxValue, longMaxValue.longValue)
    assertEquals(Long.MinValue, longMinValue.longValue)
    assertEquals(Long.MinValue, longMaxValue.add(BigDecimal.ONE).longValue)
    assertEquals(Long.MaxValue, longMinValue.subtract(BigDecimal.ONE).longValue)

    assertEquals(Long.MaxValue, longMaxValue.longValueExact)
    assertEquals(Long.MinValue, longMinValue.longValueExact)
    assertThrows(classOf[ArithmeticException], longMaxValue.add(BigDecimal.ONE).longValueExact)
    assertThrows(classOf[ArithmeticException], longMinValue.subtract(BigDecimal.ONE).longValueExact)
  }

  @Test def testSmallLongValueExact(): Unit = {
    def test(x: Long): Unit =
      assertEquals(x, new BigDecimal(x).longValueExact)

    test(0L)
    test(5L)
    test(-5L)
  }

  @Test def testLongValueExactNonWhole(): Unit = {
    def test(smallValue: Long, scale: Int): Unit = {
      val value = new BigDecimal(java.math.BigInteger.valueOf(smallValue), scale)
      assertThrows(classOf[ArithmeticException], value.longValueExact)
    }

    test(1L, 1)
    test(15L, 1)
    test(-1L, 1)
    test(-15L, 1)
  }

  @Test def bigDecimal_9_223372E285625056_should_not_be_a_valid_long_issue_2314(): Unit = {
    val num = new BigDecimal("9.223372E+285625056")

    // Sanity checks
    assertEquals(-285625050, num.scale)
    assertEquals(7, num.precision)
    assertEquals("9.223372E+285625056", num.toString)

    // Source of issue
    assertThrows(classOf[ArithmeticException], num.longValueExact)

    // Code from issue #2314
    assertFalse(scala.math.BigDecimal("9.223372E+285625056").isValidLong)
    assertFalse(scala.math.BigDecimal(10, scale = Int.MinValue).isValidLong)
    assertFalse(scala.math.BigDecimal(10, scale = Int.MaxValue).isValidLong)
  }

  @Test def testScaleByPowerOfTen1(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 13
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val result = aNumber.scaleByPowerOfTen(10)
    val res = "1231212478987482988429808779810457634781384756794.987"
    val resScale = 3
    assertEquals(result.toString, res)
    assertEquals(result.scale(), resScale, 0d)
  }

  @Test def testScaleByPowerOfTen2(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = -13
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val result = aNumber.scaleByPowerOfTen(10)
    val res = "1.231212478987482988429808779810457634781384756794987E+74"
    val resScale = -23
    assertEquals(result.toString, res)
    assertEquals(result.scale(), resScale)
  }

  @Test def testShortValue(): Unit = {
    val value = BigDecimal.valueOf(0x13fff)
    assertEquals(value.shortValue(), 0x3fff)
    expectThrows(classOf[ArithmeticException], value.shortValueExact())
  }

  @Test def testToBigIntegerExact1(): Unit = {
    val a = "-123809648392384754573567356745735.63567890295784902768787678287E+45"
    val aNumber = new BigDecimal(a)
    val res = "-123809648392384754573567356745735635678902957849027687876782870000000000000000"
    val result = aNumber.toBigIntegerExact()
    assertEquals(result.toString, res)
  }

  @Test def testToBigIntegerExactException(): Unit = {
    val a = "-123809648392384754573567356745735.63567890295784902768787678287E-10"
    val aNumber = new BigDecimal(a)
    expectThrows(classOf[ArithmeticException],  aNumber.toBigIntegerExact())
  }

  @Test def testToBigIntegerNeg1(): Unit = {
    val a = "-123809648392384754573567356745735.63567890295784902768787678287E+21"
    val bNumber = new BigInteger("-123809648392384754573567356745735635678902957849027687")
    val aNumber = new BigDecimal(a)
    val result = aNumber.toBigInteger()
    assertTrue(result == bNumber)
  }

  @Test def testToBigIntegerNeg2(): Unit = {
    val a = "-123809648392384754573567356745735.63567890295784902768787678287E+15"
    val bNumber = new BigInteger("-123809648392384754573567356745735635678902957849")
    val aNumber = new BigDecimal(a)
    val result = aNumber.toBigInteger()
    assertTrue(result == bNumber)
  }

  @Test def testToBigIntegerNeg3(): Unit = {
    val a = "-123809648392384754573567356745735.63567890295784902768787678287E+45"
    val bNumber = new BigInteger("-123809648392384754573567356745735635678902957849027687876782870000000000000000")
    val aNumber = new BigDecimal(a)
    val result = aNumber.toBigInteger()
    assertTrue(result == bNumber)
  }

  @Test def testToBigIntegerPos1(): Unit = {
    val a = "123809648392384754573567356745735.63567890295784902768787678287E+21"
    val bNumber = new BigInteger("123809648392384754573567356745735635678902957849027687")
    val aNumber = new BigDecimal(a)
    val result = aNumber.toBigInteger()
    assertTrue(result == bNumber)
  }

  @Test def testToBigIntegerPos2(): Unit = {
    val a = "123809648392384754573567356745735.63567890295784902768787678287E+15"
    val bNumber = new BigInteger("123809648392384754573567356745735635678902957849")
    val aNumber = new BigDecimal(a)
    val result = aNumber.toBigInteger()
    assertTrue(result == bNumber)
  }

  @Test def testToBigIntegerPos3(): Unit = {
    val a = "123809648392384754573567356745735.63567890295784902768787678287E+45"
    val bNumber = new BigInteger("123809648392384754573567356745735635678902957849027687876782870000000000000000")
    val aNumber = new BigDecimal(a)
    val result = aNumber.toBigInteger()
    assertTrue(result == bNumber)
  }

  @Test def testToBigIntegerZero(): Unit = {
    val a = "-123809648392384754573567356745735.63567890295784902768787678287E-500"
    val bNumber = new BigInteger("0")
    val aNumber = new BigDecimal(a)
    val result = aNumber.toBigInteger()
    assertTrue(result == bNumber)
  }

  @Test def testToEngineeringStringNeg(): Unit = {
    val a = "-123809648392384754573567356745735.63567890295784902768787678287E-501"
    val aNumber = new BigDecimal(a)
    val result = "-123.80964839238475457356735674573563567890295784902768787678287E-471"
    assertEquals(aNumber.toEngineeringString(), result)
  }

  @Test def testToEngineeringStringPos(): Unit = {
    val a = "123809648392384754573567356745735.63567890295784902768787678287E-501"
    val aNumber = new BigDecimal(a)
    val result = "123.80964839238475457356735674573563567890295784902768787678287E-471"
    assertEquals(aNumber.toEngineeringString(), result)
  }

  @Test def testToEngineeringStringZeroNegExponent(): Unit = {
    val a = "0.0E-16"
    val aNumber = new BigDecimal(a)
    val result = "0.00E-15"
    assertEquals(aNumber.toEngineeringString(), result)
  }

  @Test def testToEngineeringStringZeroPosExponent(): Unit = {
    val a = "0.0E+16"
    val aNumber = new BigDecimal(a)
    val result = "0E+15"
    assertEquals(aNumber.toEngineeringString(), result)
  }

  @Test def testToPlainStringNegNegExp(): Unit = {
    val a = "-123809648392384754573567356745735.63567890295784902768787678287E-100"
    val aNumber = new BigDecimal(a)
    val result = "-0.00000000000000000000000000000000000000000000000000000" +
      "0000000000000012380964839238475457356735674573563567890295784902768" +
      "787678287"
    assertTrue(aNumber.toPlainString() == result)
  }

  @Test def testToPlainStringNegPosExp(): Unit = {
    val a = "-123809648392384754573567356745735.63567890295784902768787678287E100"
    val aNumber = new BigDecimal(a)
    val result = "-1238096483923847545735673567457356356789029578490276878" +
      "7678287000000000000000000000000000000000000000000000000000000000000" +
      "00000000000"
    assertTrue(aNumber.toPlainString() == result)
  }

  @Test def testToPlainStringPosNegExp(): Unit = {
    val a = "123809648392384754573567356745735.63567890295784902768787678287E-100"
    val aNumber = new BigDecimal(a)
    val result = "0.000000000000000000000000000000000000000000000000000000" +
      "0000000000000123809648392384754573567356745735635678902957849027687" +
      "87678287"
    assertTrue(aNumber.toPlainString() == result)
  }

  @Test def testToPlainStringPosPosExp(): Unit = {
    val a = "123809648392384754573567356745735.63567890295784902768787678287E+100"
    val aNumber = new BigDecimal(a)
    val result = "12380964839238475457356735674573563567890295784902768787" +
      "6782870000000000000000000000000000000000000000000000000000000000000" +
      "0000000000"
    assertTrue(aNumber.toPlainString() == result)
  }

  @Test def testToStringNeg(): Unit = {
    val a = "-123.4564563673567380964839238475457356735674573563567890295784902768787678287E-5"
    val aNumber = new BigDecimal(a)
    val result = "-0.001234564563673567380964839238475457356735674573563567890295784902768787678287"
    assertTrue(aNumber.toString == result)
  }

  @Test def testToStringPos(): Unit = {
    val a = "123809648392384754573567356745735.63567890295784902768787678287E-500"
    val aNumber = new BigDecimal(a)
    val result = "1.2380964839238475457356735674573563567890295784902768787678287E-468"
    assertTrue(aNumber.toString == result)
  }

  @Test def testToStringZeroScale(): Unit = {
    val a = "-123809648392384754573567356745735635678902957849027687876782870"
    val aNumber = new BigDecimal(new BigInteger(a))
    val result = "-123809648392384754573567356745735635678902957849027687876782870"
    assertTrue(aNumber.toString == result)
  }

  @Test def testValueOfDoubleNaN(): Unit = {
    val a = Double.NaN
    expectThrows(classOf[NumberFormatException],  BigDecimal.valueOf(a))
  }

  @Test def testValueOfDoubleNeg(): Unit = {
    val a = -65678765876567576.98788767
    val result = BigDecimal.valueOf(a)
    val res = "-65678765876567576"
    val resScale = 0
    assertEquals(result.toString, res)
    assertEquals(result.scale(), resScale)
  }

  @Test def testValueOfDoublePos1(): Unit = {
    val a = 65678765876567576.98788767
    val result = BigDecimal.valueOf(a)
    val res = "65678765876567576"
    val resScale = 0
    assertEquals(result.toString, res)
    assertEquals(result.scale(), resScale)
  }

  @Test def testValueOfDoublePos2(): Unit = {
    val a = 12321237576.98788767
    val result = BigDecimal.valueOf(a)
    val res = "12321237576.987888"
    val resScale = 6
    assertEquals(result.toString, res)
    assertEquals(result.scale(), resScale)
  }

  @Test def testValueOfDoublePos3(): Unit = {
    val a = 12321237576.9878838
    val result = BigDecimal.valueOf(a)
    val res = "12321237576.98788"
    val resScale = 6
    assertTrue(result.toString.startsWith(res))
    assertEquals(result.scale(), resScale)
  }

  @Test def testValueOfNegScaleNeg(): Unit = {
    val a = -98374823947823578L
    val scale = -12
    val aNumber = BigDecimal.valueOf(a, scale)
    val result = "-9.8374823947823578E+28"
    assertTrue(aNumber.toString == result)
  }

  @Test def testValueOfNegScalePos(): Unit = {
    val a = -98374823947823578L
    val scale = 12
    val aNumber = BigDecimal.valueOf(a, scale)
    val result = "-98374.823947823578"
    assertTrue(aNumber.toString == result)
  }

  @Test def testValueOfNegZeroScale(): Unit = {
    val a = -98374823947823578L
    val aNumber = BigDecimal.valueOf(a)
    val result = "-98374823947823578"
    assertTrue(aNumber.toString == result)
  }

  @Test def testValueOfPosScaleNeg(): Unit = {
    val a = 98374823947823578L
    val scale = -12
    val aNumber = BigDecimal.valueOf(a, scale)
    val result = "9.8374823947823578E+28"
    assertTrue(aNumber.toString == result)
  }

  @Test def testValueOfPosScalePos(): Unit = {
    val a = 98374823947823578L
    val scale = 12
    val aNumber = BigDecimal.valueOf(a, scale)
    val result = "98374.823947823578"
    assertTrue(aNumber.toString == result)
  }

  @Test def testValueOfPosZeroScale(): Unit = {
    val a = 98374823947823578L
    val aNumber = BigDecimal.valueOf(a)
    val result = "98374823947823578"
    assertTrue(aNumber.toString == result)
  }

  @Test def testValueOfZeroScaleNeg(): Unit = {
    val scale = -2
    val number = BigDecimal.valueOf(0L, scale)
    assertEquals(number.toString, "0E+2")
    assertEquals(number.scale(), scale)
  }

  @Test def testValueOfZeroScalePos(): Unit = {
    val scale = 1
    val number = BigDecimal.valueOf(0L, scale)
    assertEquals(number.toString, "0.0")
    assertEquals(number.scale(), scale)
  }
}
