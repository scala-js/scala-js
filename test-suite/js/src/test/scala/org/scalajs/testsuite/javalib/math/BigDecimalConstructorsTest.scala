// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigDecimalConstructorsTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math._

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

class BigDecimalConstructorsTest {

  import BigDecimalConstructorsTest._

  @Test def testConstrBI(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val bA = new BigInteger(a)
    val aNumber = new BigDecimal(bA)
    assertTrue(aNumber.unscaledValue() == bA)
    assertEquals(0, aNumber.scale())
    expectThrows(classOf[NullPointerException], new BigDecimal(null.asInstanceOf[BigInteger]))
  }

  @Test def testConstrBigIntegerMathContext(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val bA = new BigInteger(a)
    val precision = 46
    val rm = RoundingMode.UP
    val mc = new MathContext(precision, rm)
    val res = "1231212478987482988429808779810457634781384757"
    val resScale = -6
    val result = new BigDecimal(bA, mc)
    assertEquals(res, result.unscaledValue().toString)
    assertEquals(resScale, result.scale())
  }

  @Test def testConstrBigIntegerScaleMathContext(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val bA = new BigInteger(a)
    val aScale = 10
    val precision = 46
    val rm = RoundingMode.CEILING
    val mc = new MathContext(precision, rm)
    val res = "1231212478987482988429808779810457634781384757"
    val resScale = 4
    val result = new BigDecimal(bA, aScale, mc)
    assertEquals(res, result.unscaledValue().toString)
    assertEquals(resScale, result.scale())
  }

  @Test def testConstrBIScale(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val bA = new BigInteger(a)
    val aScale = 10
    val aNumber = new BigDecimal(bA, aScale)
    assertTrue(aNumber.unscaledValue() == bA)
    assertEquals(aScale, aNumber.scale())
  }

  @Test def testConstrChar(): Unit = {
    val value = Array('-', '1', '2', '3', '8', '0', '.', '4', '7', '3', '8', 'E', '-', '4', '2', '3')
    val result = new BigDecimal(value)
    val res = "-1.23804738E-419"
    val resScale = 427
    assertEquals(result.toString, res)
    assertEquals(result.scale(), resScale)
    expectThrows(classOf[NumberFormatException], new BigDecimal(Array[Char]()))
  }

  @Test def testConstrCharIntInt(): Unit = {
    val value = Array('-', '1', '2', '3', '8', '0', '.', '4', '7', '3', '8', 'E', '-', '4', '2', '3')
    val offset = 3
    val len = 12
    val result = new BigDecimal(value, offset, len)
    val res = "3.804738E-40"
    val resScale = 46
    assertEquals(result.toString, res)
    assertEquals(result.scale(), resScale)
    expectThrows(classOf[NumberFormatException], new BigDecimal(Array[Char](), 0, 0))
  }

  @Test def testConstrCharIntIntMathContext(): Unit = {
    val value = Array('-', '1', '2', '3', '8', '0', '.', '4', '7', '3', '8', 'E', '-', '4', '2', '3')
    val offset = 3
    val len = 12
    val precision = 4
    val rm = RoundingMode.CEILING
    val mc = new MathContext(precision, rm)
    val result = new BigDecimal(value, offset, len, mc)
    val res = "3.805E-40"
    val resScale = 43
    assertEquals(result.toString, res)
    assertEquals(result.scale(), resScale)
    expectThrows(classOf[NumberFormatException],
        new BigDecimal(Array(), 0, 0, MathContext.DECIMAL32))
  }

  @Test def testConstrCharIntIntMathContextException1(): Unit = {
    val value = Array('-', '1', '2', '3', '8', '0', '.', '4', '7', '3', '8', 'E', '-', '4', '2', '3')
    val offset = 3
    val len = 120
    val precision = 4
    val rm = RoundingMode.CEILING
    val mc = new MathContext(precision, rm)
    expectThrows(classOf[NumberFormatException], new BigDecimal(value, offset, len, mc))
  }

  @Test def testConstrCharIntIntMathContextException2(): Unit = {
    val value = Array('-', '1', '2', '3', '8', '0', ',', '4', '7', '3', '8', 'E', '-', '4', '2', '3')
    val offset = 3
    val len = 120
    val precision = 4
    val rm = RoundingMode.CEILING
    val mc = new MathContext(precision, rm)
    expectThrows(classOf[NumberFormatException], new BigDecimal(value, offset, len, mc))
  }

  @Test def testConstrCharMathContext(): Unit = {
    val value = Array('3', '8', '0', '.', '4', '7', '3', '8', 'E', '-', '4', '2')
    val precision = 4
    val rm = RoundingMode.CEILING
    val mc = new MathContext(precision, rm)
    val result = new BigDecimal(value, mc)
    val res = "3.805E-40"
    val resScale = 43
    assertEquals(result.toString, res)
    assertEquals(result.scale(), resScale)
    expectThrows(classOf[NumberFormatException], new BigDecimal(Array[Char](), MathContext.DECIMAL32))
  }

  @Test def testConstrDouble(): Unit = {
    val a = 732546982374982347892379283571094797.287346782359284756
    val aNumber = new BigDecimal(a)
    val expected = new BigDecimal("732546982374982347892379283571094797.287346782359284756")
    assertTrue(aNumber.minus(expected) < 1E21)
  }

  @Test def testConstrDouble01(): Unit = {
    val a: Double = 1.0e-1
    val aNumber = new BigDecimal(a)
    val expected = new BigDecimal(".1000000000000000055511151231257827021181583404541015625")
    assertTrue(aNumber.minus(expected) < 1e-9)
  }

  @Test def testConstrDouble02(): Unit = {
    val a: Double = 0.555
    val aNumber = new BigDecimal(a)
    val expected = new BigDecimal(".55500000000000004884981308350688777863979339599609375")
    assertTrue(aNumber.minus(expected) < 1e-8)
  }

  @Test def testConstrDoubleDenormalized(): Unit = {
    //INF
    val a: Double = 2.274341322658976E-304
    val aNumber = new BigDecimal(a)
    val expected = new BigDecimal("2.274341322658976E-304")
    assertTrue(aNumber.minus(expected) < 1e-305)
  }

  @Test def testConstrDoubleMathContext(): Unit = {
    val a: Double = 732546982374982347892379283571094797.287346782359284756
    val precision = 21
    val rm = RoundingMode.CEILING
    val mc = new MathContext(precision, rm)
    val result = new BigDecimal(a, mc)
    val expected = new BigDecimal("732546982374982e21")
    assertTrue(result.minus(expected) < 1e21)
    expectThrows(classOf[NumberFormatException], new BigDecimal(Double.NaN))
    expectThrows(classOf[NumberFormatException],
        new BigDecimal(Double.PositiveInfinity))
    expectThrows(classOf[NumberFormatException],
        new BigDecimal(Double.NegativeInfinity))
  }

  @Test def testConstrDoubleMinus01(): Unit = {
    val a: Double = -1.0e-1
    val aNumber = new BigDecimal(a)
    val expected = new BigDecimal("-.1000000000000000055511151231257827021181583404541015625")
    assertTrue(aNumber.minus(expected) < 1e-9)
  }

  @Test def testConstrDoubleNaN(): Unit = {
    val a: Double = Double.NaN
    expectThrows(classOf[NumberFormatException], new BigDecimal(a))
  }

  @Test def testConstrDoubleNegInfinity(): Unit = {
    val a: Double = Double.NegativeInfinity
    expectThrows(classOf[NumberFormatException], new BigDecimal(a))
  }

  @Test def testConstrDoublePosInfinity(): Unit = {
    val a: Double = Double.PositiveInfinity
    expectThrows(classOf[NumberFormatException], new BigDecimal(a))
  }

  @Test def testConstrInt(): Unit = {
    val a = 732546982
    val res = "732546982"
    val resScale = 0
    val result = new BigDecimal(a)
    assertEquals(res, result.unscaledValue().toString)
    assertEquals(resScale, result.scale())
  }

  @Test def testConstrIntMathContext(): Unit = {
    val a = 732546982
    val precision = 21
    val rm = RoundingMode.CEILING
    val mc = new MathContext(precision, rm)
    val res = "732546982"
    val resScale = 0
    val result = new BigDecimal(a, mc)
    assertEquals(res, result.unscaledValue().toString)
    assertEquals(resScale, result.scale())
  }

  @Test def testConstrLong(): Unit = {
    val a: Long = 4576578677732546982L
    val res = "4576578677732546982"
    val resScale = 0
    val result = new BigDecimal(a)
    assertEquals(res, result.unscaledValue().toString)
    assertEquals(resScale, result.scale())
  }

  @Test def testConstrLongMathContext(): Unit = {
    val a: Long = 4576578677732546982L
    val precision = 5
    val rm = RoundingMode.CEILING
    val mc = new MathContext(precision, rm)
    val res = "45766"
    val resScale = -14
    val result = new BigDecimal(a, mc)
    assertEquals(res, result.unscaledValue().toString)
    assertEquals(resScale, result.scale())
  }

  @Test def testConstrStringException(): Unit = {
    val a = "-238768.787678287a+10"
    expectThrows(classOf[NumberFormatException], new BigDecimal(a))
  }

  @Test def testConstrStringExceptionEmptyExponent1(): Unit = {
    val a = "-238768.787678287e"
    expectThrows(classOf[NumberFormatException], new BigDecimal(a))
  }

  @Test def testConstrStringExceptionEmptyExponent2(): Unit = {
    val a = "-238768.787678287e-"
    expectThrows(classOf[NumberFormatException], new BigDecimal(a))
  }

  @Test def testConstrStringExceptionExponentGreaterIntegerMax(): Unit = {
    val a = "-238768.787678287e214748364767876"
    expectThrows(classOf[NumberFormatException], new BigDecimal(a))
  }

  @Test def testConstrStringExceptionExponentLessIntegerMin(): Unit = {
    val a = "-238768.787678287e-214748364767876"
    expectThrows(classOf[NumberFormatException], new BigDecimal(a))
  }

  @Test def testConstrStringExponentIntegerMax(): Unit = {
    val a = "-238768.787678287e2147483647"
    val aScale = -2147483638
    val bA = new BigInteger("-238768787678287")
    val aNumber = new BigDecimal(a)
    assertTrue(aNumber.unscaledValue() == bA)
    assertEquals(aScale, aNumber.scale())
  }

  @Test def testConstrStringExponentIntegerMin(): Unit = {
    val a = ".238768e-2147483648"
    expectThrows(classOf[NumberFormatException], new BigDecimal(a))
  }

  @Test def testConstrStringMultipleSignsStartWithPlus(): Unit = {
    val a = "+-3"
    expectThrows(classOf[NumberFormatException], new BigDecimal(a))
  }

  @Test def testConstrStringMultipleSignsStartWithMinus(): Unit = {
    val a = "-+3"
    expectThrows(classOf[NumberFormatException], new BigDecimal(a))
  }

  @Test def testConstrStringMathContext(): Unit = {
    val a = "-238768787678287e214"
    val precision = 5
    val rm = RoundingMode.CEILING
    val mc = new MathContext(precision, rm)
    val res = "-23876"
    val resScale = -224
    val result = new BigDecimal(a, mc)
    assertEquals(result.unscaledValue().toString, res)
    assertEquals(resScale, result.scale())
  }

  @Test def testConstrStringWithExponentWithoutPoint1(): Unit = {
    val a = "-238768787678287e214"
    val aScale = -214
    val bA = new BigInteger("-238768787678287")
    val aNumber = new BigDecimal(a)
    assertTrue(aNumber.unscaledValue() == bA)
    assertEquals(aScale, aNumber.scale())
  }

  @Test def testConstrStringWithExponentWithoutPoint2(): Unit = {
    val a = "-238768787678287e-214"
    val aScale = 214
    val bA = new BigInteger("-238768787678287")
    val aNumber = new BigDecimal(a)
    assertTrue(aNumber.unscaledValue() == bA)
    assertEquals(aScale, aNumber.scale())
  }

  @Test def testConstrStringWithExponentWithoutPoint3(): Unit = {
    val a = "238768787678287e-214"
    val aScale = 214
    val bA = new BigInteger("238768787678287")
    val aNumber = new BigDecimal(a)
    assertTrue(aNumber.unscaledValue() == bA)
    assertEquals(aScale, aNumber.scale())
  }

  @Test def testConstrStringWithExponentWithoutPoint4(): Unit = {
    val a = "238768787678287e+214"
    val aScale = -214
    val bA = new BigInteger("238768787678287")
    val aNumber = new BigDecimal(a)
    assertTrue(aNumber.unscaledValue() == bA)
    assertEquals(aScale, aNumber.scale())
  }

  @Test def testConstrStringWithExponentWithoutPoint5(): Unit = {
    val a = "238768787678287E214"
    val aScale = -214
    val bA = new BigInteger("238768787678287")
    val aNumber = new BigDecimal(a)
    assertTrue(aNumber.unscaledValue() == bA)
    assertEquals(aScale, aNumber.scale())
  }

  @Test def testConstrStringWithExponentWithPoint1(): Unit = {
    val a = "23985439837984782435652424523876878.7678287e+214"
    val aScale = -207
    val bA = new BigInteger("239854398379847824356524245238768787678287")
    val aNumber = new BigDecimal(a)
    assertTrue(aNumber.unscaledValue() == bA)
    assertEquals(aScale, aNumber.scale())
  }

  @Test def testConstrStringWithExponentWithPoint2(): Unit = {
    val a = "238096483923847545735673567457356356789029578490276878.7678287e-214"
    val aScale = 221
    val bA = new BigInteger("2380964839238475457356735674573563567890295784902768787678287")
    val aNumber = new BigDecimal(a)
    assertTrue(aNumber.unscaledValue() == bA)
    assertEquals(aScale, aNumber.scale())
  }

  @Test def testConstrStringWithExponentWithPoint3(): Unit = {
    val a = "2380964839238475457356735674573563567890.295784902768787678287E+21"
    val aScale = 0
    val bA = new BigInteger("2380964839238475457356735674573563567890295784902768787678287")
    val aNumber = new BigDecimal(a)
    assertTrue(aNumber.unscaledValue() == bA)
    assertEquals(aScale, aNumber.scale())
  }

  @Test def testConstrStringWithExponentWithPoint4(): Unit = {
    val a = "23809648392384754573567356745735635678.90295784902768787678287E+21"
    val aScale = 2
    val bA = new BigInteger("2380964839238475457356735674573563567890295784902768787678287")
    val aNumber = new BigDecimal(a)
    assertTrue(aNumber.unscaledValue() == bA)
    assertEquals(aScale, aNumber.scale())
  }

  @Test def testConstrStringWithExponentWithPoint5(): Unit = {
    val a = "238096483923847545735673567457356356789029.5784902768787678287E+21"
    val aScale = -2
    val bA = new BigInteger("2380964839238475457356735674573563567890295784902768787678287")
    val aNumber = new BigDecimal(a)
    assertTrue(aNumber.unscaledValue() == bA)
    assertEquals(aScale, aNumber.scale())
  }

  @Test def testConstrStringWithLeadingZeros(): Unit = {
    assertEquals(1, new BigDecimal("-000.1").precision())
    assertEquals(4, new BigDecimal("001234").precision())
    assertEquals(4, new BigDecimal("-5555").precision())
    assertEquals(1, new BigDecimal("0").precision())
    assertEquals(1, new BigDecimal("-0").precision())
    assertEquals(4, new BigDecimal("001234e3").precision())
    assertEquals(5, new BigDecimal("00056789e+17").precision())
    assertEquals(6, new BigDecimal("0900000e-42").precision())
  }

  @Test def testConstrStringWithoutExpNeg(): Unit = {
    val a = "-732546982374982347892379283571094797.287346782359284756"
    val aScale = 18
    val bA = new BigInteger("-732546982374982347892379283571094797287346782359284756")
    val aNumber = new BigDecimal(a)
    assertTrue(bA == aNumber.unscaledValue())
    assertEquals(aScale, aNumber.scale())
  }

  @Test def testConstrStringWithoutExpPos1(): Unit = {
    val a = "732546982374982347892379283571094797.287346782359284756"
    val aScale = 18
    val bA = new BigInteger("732546982374982347892379283571094797287346782359284756")
    val aNumber = new BigDecimal(a)
    assertTrue(aNumber.unscaledValue() == bA)
    assertEquals(aNumber.scale(), aScale)
  }

  @Test def testConstrStringWithoutExpPos2(): Unit = {
    val a = "+732546982374982347892379283571094797.287346782359284756"
    val aScale = 18
    val bA = new BigInteger("732546982374982347892379283571094797287346782359284756")
    val aNumber = new BigDecimal(a)
    assertTrue(aNumber.unscaledValue() == bA)
    assertEquals(aNumber.scale(), aScale)
  }

  @Test def testConstrStringWithoutExpWithoutPoint(): Unit = {
    val a = "-732546982374982347892379283571094797287346782359284756"
    val aScale = 0
    val bA = new BigInteger("-732546982374982347892379283571094797287346782359284756")
    val aNumber = new BigDecimal(a)
    assertTrue(bA == aNumber.unscaledValue())
    assertEquals(aNumber.scale(), aScale)
  }

  @Test def testConstrZero(): Unit = {
    var bd = new BigDecimal("0")
    assertEquals(0, bd.intValueExact())
    assertEquals(1, bd.precision())
    assertEquals(0, bd.scale())
    bd = new BigDecimal("0.0")
    assertEquals(0, bd.intValueExact())
    assertEquals(1, bd.precision())
    assertEquals(1, bd.scale())
    bd = new BigDecimal("0.00")
    assertEquals(0, bd.intValueExact())
    assertEquals(1, bd.precision())
    assertEquals(2, bd.scale())
  }

  @Test def testFieldONE(): Unit = {
    val oneS = "1"
    val oneD = 1.0
    assertEquals(oneS, BigDecimal.ONE.toString)
    assertEquals(oneD, BigDecimal.ONE.doubleValue(), 0d)
  }

  @Test def testFieldTEN(): Unit = {
    val oneS = "10"
    val oneD = 10.0
    assertEquals(oneS, BigDecimal.TEN.toString)
    assertEquals(oneD, BigDecimal.TEN.doubleValue(), 0d)
  }

  @Test def testFieldZERO(): Unit = {
    val oneS = "0"
    val oneD = 0.0
    assertEquals(oneS, BigDecimal.ZERO.toString)
    assertEquals(oneD, BigDecimal.ZERO.doubleValue(), 0d)
  }
}

object BigDecimalConstructorsTest {
  // Helper for comparing values within a certain +/- delta
  implicit class BigDecimalOps(val actual: BigDecimal) extends AnyVal {
    def minus(expected: BigDecimal): Double = {
      val actualDeltaDecimal:BigDecimal = actual.subtract(expected)
      val actualDelta = actualDeltaDecimal.abs().doubleValue()
      actualDelta
    }
  }
}
