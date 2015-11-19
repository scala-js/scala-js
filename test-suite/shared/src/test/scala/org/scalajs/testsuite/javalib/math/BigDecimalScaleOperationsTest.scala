// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigDecimalScaleOperationsTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math._

import org.junit.Test
import org.junit.Assert._

class  BigDecimalScaleOperationsTest {

  @Test def testScaleByPowerOfTen(): Unit = {
    val bd = BigDecimal.ONE.scaleByPowerOfTen(1)
    assertEquals(bd.intValue(), 10)
  }

  @Test def testScaleDefault(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val cScale = 0
    val aNumber = new BigDecimal(new BigInteger(a))
    assertTrue(aNumber.scale() == cScale)
  }

  @Test def testScaleNeg(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = -10
    val cScale = -10
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    assertTrue(aNumber.scale() == cScale)
  }

  @Test def testScalePos(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 10
    val cScale = 10
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    assertTrue(aNumber.scale() == cScale)
  }

  @Test def testScaleZero(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 0
    val cScale = 0
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    assertTrue(aNumber.scale() == cScale)
  }

  @Test def testUnscaledValue(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 100
    val bNumber = new BigInteger(a)
    val aNumber = new BigDecimal(bNumber, aScale)
    val aNumberUnscaledValue:BigInteger = aNumber.unscaledValue()
    assertTrue(aNumberUnscaledValue == bNumber)
    assertTrue(aNumber.unscaledValue() == bNumber)
  }

  @Test def testUnscaledValue2(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 100
    val bNumber = new BigInteger(a)
    val aNumber = new BigDecimal(bNumber, aScale)
    assertTrue(aNumber.unscaledValue() == bNumber)
  }

  @Test def testSetScaleGreater(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 18
    val newScale = 28
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = aNumber.setScale(newScale)
    assertTrue(bNumber.scale() == newScale)
    assertEquals(bNumber.compareTo(aNumber), 0)
  }

  @Test def testSetScaleLess(): Unit = {
    val a = "2.345726458768760000E+10"
    val newScale = 5
    val aNumber = new BigDecimal(a)
    val bNumber = aNumber.setScale(newScale)
    assertTrue(bNumber.scale() == newScale)
    assertEquals(bNumber.compareTo(aNumber), 0)
  }

  @Test def testSetScaleException(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 28
    val newScale = 18
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    try {
    aNumber.setScale(newScale)
    fail()
  } catch {
     case _: Throwable => // As expected
  }
  }

  @Test def testSetScaleSame(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 18
    val newScale = 18
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = aNumber.setScale(newScale)
    assertTrue(bNumber.scale() == newScale)
    assertTrue(bNumber == aNumber)
  }

  @Test def testSetScaleRoundUp(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val b = "123121247898748298842980877981045763478139"
    val aScale = 28
    val newScale = 18
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = aNumber.setScale(newScale, BigDecimal.ROUND_UP)
    assertEquals(newScale, bNumber.scale())
    assertEquals(b, bNumber.unscaledValue().toString)
  }

  @Test def testSetScaleRoundDown(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val b = "123121247898748298842980877981045763478138"
    val aScale = 28
    val newScale = 18
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = aNumber.setScale(newScale, BigDecimal.ROUND_DOWN)
    assertTrue(bNumber.scale() == newScale)
    assertTrue(bNumber.unscaledValue().toString == b)
  }

  @Test def testSetScaleRoundCeiling(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val b = "123121247898748298842980877981045763478139"
    val aScale = 28
    val newScale = 18
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = aNumber.setScale(newScale, BigDecimal.ROUND_CEILING)
    assertTrue(bNumber.scale() == newScale)
    assertTrue(bNumber.unscaledValue().toString == b)
  }

  @Test def testSetScaleRoundFloor(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val b = "123121247898748298842980877981045763478138"
    val aScale = 28
    val newScale = 18
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = aNumber.setScale(newScale, BigDecimal.ROUND_FLOOR)
    assertTrue(bNumber.scale() == newScale)
    assertTrue(bNumber.unscaledValue().toString == b)
  }

  @Test def testSetScaleRoundHalfUp(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val b = "123121247898748298842980877981045763478138"
    val aScale = 28
    val newScale = 18
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = aNumber.setScale(newScale, BigDecimal.ROUND_HALF_UP)
    assertTrue(bNumber.scale() == newScale)
    assertTrue(bNumber.unscaledValue().toString == b)
  }

  @Test def testSetScaleRoundHalfDown(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val b = "123121247898748298842980877981045763478138"
    val aScale = 28
    val newScale = 18
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = aNumber.setScale(newScale, BigDecimal.ROUND_HALF_DOWN)
    assertTrue(bNumber.scale() == newScale)
    assertTrue(bNumber.unscaledValue().toString == b)
  }

  @Test def testSetScaleRoundHalfEven(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val b = "123121247898748298842980877981045763478138"
    val aScale = 28
    val newScale = 18
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = aNumber.setScale(newScale, BigDecimal.ROUND_HALF_EVEN)
    assertTrue(bNumber.scale() == newScale)
    assertTrue(bNumber.unscaledValue().toString == b)
  }

  @Test def testSetScaleIntRoundingMode(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 28
    val newScale = 18
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val result = aNumber.setScale(newScale, RoundingMode.HALF_EVEN)
    val res = "123121247898748298842980.877981045763478138"
    val resScale = 18
    assertEquals(result.toString, res)
    assertEquals(result.scale(), resScale)
  }

  @Test def testMovePointLeftPos(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 28
    val shift = 18
    val resScale = 46
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = aNumber.movePointLeft(shift)
    assertTrue(bNumber.scale() == resScale)
    assertTrue(bNumber.unscaledValue().toString == a)
  }

  @Test def testMovePointLeftNeg(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 28
    val shift = -18
    val resScale = 10
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = aNumber.movePointLeft(shift)
    assertTrue(bNumber.scale() == resScale)
    assertTrue(bNumber.unscaledValue().toString == a)
  }

  @Test def testMovePointRightPosGreater(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 28
    val shift = 18
    val resScale = 10
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = aNumber.movePointRight(shift)
    assertTrue(bNumber.scale() == resScale)
    assertTrue(bNumber.unscaledValue().toString == a)
  }

  @Test def testMovePointRightPosLess(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val b = "123121247898748298842980877981045763478138475679498700"
    val aScale = 28
    val shift = 30
    val resScale = 0
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = aNumber.movePointRight(shift)
    assertEquals(resScale, bNumber.scale())
    assertEquals(b, bNumber.unscaledValue().toString)
  }

  @Test def testMovePointRightNeg(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 28
    val shift = -18
    val resScale = 46
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = aNumber.movePointRight(shift)
    assertEquals(resScale, bNumber.scale())
    assertEquals(a, bNumber.unscaledValue().toString)
  }

  @Test def testMovePointRightException(): Unit = {
    val a = "12312124789874829887348723648726347429808779810457634781384756794987"
    val aScale = Int.MaxValue
    val shift = -18
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    try {
    aNumber.movePointRight(shift)
    fail()
  } catch {
     case _: Throwable => // As expected
  }
  }

  @Test def testPrecision(): Unit = {
    val a = "12312124789874829887348723648726347429808779810457634781384756794987"
    val aScale = 14
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val prec = aNumber.precision()
    assertEquals(prec, 68)
  }
}
