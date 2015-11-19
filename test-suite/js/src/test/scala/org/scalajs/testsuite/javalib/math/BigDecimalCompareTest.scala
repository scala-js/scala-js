// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigDecimalCompareTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math._

import org.junit.Test
import org.junit.Assert._

class BigDecimalCompareTest {

  @Test def testAbsMathContextNeg(): Unit = {
    val a = "-123809648392384754573567356745735.63567890295784902768787678287E+21"
    val aNumber = new BigDecimal(a)
    val precision = 15
    val rm = RoundingMode.HALF_DOWN
    val mc = new MathContext(precision, rm)
    val result = "1.23809648392385E+53"
    val resScale = -39
    val res = aNumber.abs(mc)
    assertEquals(res.toString, result)
    assertEquals(res.scale(), resScale)
  }

  @Test def testAbsMathContextPos(): Unit = {
    val a = "123809648392384754573567356745735.63567890295784902768787678287E+21"
    val aNumber = new BigDecimal(a)
    val precision = 41
    val rm = RoundingMode.HALF_EVEN
    val mc = new MathContext(precision, rm)
    val result = "1.2380964839238475457356735674573563567890E+53"
    val resScale = -13
    val res = aNumber.abs(mc)
    assertEquals(res.toString, result)
    assertEquals(res.scale(), resScale)
  }

  @Test def testAbsNeg(): Unit = {
    val a = "-123809648392384754573567356745735.63567890295784902768787678287E+21"
    val aNumber = new BigDecimal(a)
    val result = "123809648392384754573567356745735635678902957849027687.87678287"
    assertEquals(result, aNumber.abs().toString)
  }

  @Test def testAbsPos(): Unit = {
    val a = "123809648392384754573567356745735.63567890295784902768787678287E+21"
    val aNumber = new BigDecimal(a)
    val result = "123809648392384754573567356745735635678902957849027687.87678287"
    assertEquals(result, aNumber.abs().toString)
  }

  @Test def testCompareEqualScale1(): Unit = {
    val a = "12380964839238475457356735674573563567890295784902768787678287"
    val aScale = 18
    val b = "4573563567890295784902768787678287"
    val bScale = 18
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = 1
    assertEquals(result, aNumber.compareTo(bNumber))
  }

  @Test def testCompareEqualScale2(): Unit = {
    val a = "12380964839238475457356735674573563567890295784902768787678287"
    val aScale = 18
    val b = "4573563923487289357829759278282992758247567890295784902768787678287"
    val bScale = 18
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = -1
    assertEquals(result, aNumber.compareTo(bNumber))
  }

  @Test def testCompareGreaterScale1(): Unit = {
    val a = "12380964839238475457356735674573563567890295784902768787678287"
    val aScale = 28
    val b = "4573563567890295784902768787678287"
    val bScale = 18
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = 1
    assertEquals(result, aNumber.compareTo(bNumber))
  }

  @Test def testCompareGreaterScale2(): Unit = {
    val a = "12380964839238475457356735674573563567890295784902768787678287"
    val aScale = 48
    val b = "4573563567890295784902768787678287"
    val bScale = 2
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = -1
    assertEquals(result, aNumber.compareTo(bNumber))
  }

  @Test def testCompareLessScale1(): Unit = {
    val a = "12380964839238475457356735674573563567890295784902768787678287"
    val aScale = 18
    val b = "4573563567890295784902768787678287"
    val bScale = 28
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = 1
    assertEquals(result, aNumber.compareTo(bNumber))
  }

  @Test def testCompareLessScale2(): Unit = {
    val a = "12380964839238475457356735674573"
    val aScale = 36
    val b = "45735635948573894578349572001798379183767890295784902768787678287"
    val bScale = 48
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = -1
    assertEquals(result, aNumber.compareTo(bNumber))
  }

  @Test def testEqualsEqual(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = -24
    val b = "92948782094488478231212478987482988429808779810457634781384756794987"
    val bScale = -24
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    assertTrue(aNumber == bNumber)
  }

  @Test def testEqualsNull(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = -24
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    assertFalse(aNumber == null)
  }

  @Test def testEqualsUnequal1(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = -24
    val b = "7472334223847623782375469293018787918347987234564568"
    val bScale = 13
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    assertFalse(aNumber == bNumber)
  }

  @Test def testEqualsUnequal2(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = -24
    val b = "92948782094488478231212478987482988429808779810457634781384756794987"
    val bScale = 13
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    assertFalse(aNumber == bNumber)
  }

  @Test def testEqualsUnequal3(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = -24
    val b = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    // changed '==' to 'equals' to remove compiler warning
    assertFalse(aNumber.equals(b))
  }

  @Test def testFractionScale(): Unit = {
    var a = new BigDecimal("0.02")
    var b = new BigDecimal("0.02000")
    assertEquals(a.compareTo(b), 0)
    val a1 = new BigDecimal("0.029900000000000003")
    val a2 = new BigDecimal("0.0001")
    a = a1.add(a2)
    b = new BigDecimal("0.03990")
    assertEquals(a.compareTo(b), -1)
  }

  @Test def testHashCodeEqual(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = -24
    val b = "92948782094488478231212478987482988429808779810457634781384756794987"
    val bScale = -24
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    assertEquals(bNumber.hashCode, aNumber.hashCode)
  }

  @Test def testHashCodeUnequal(): Unit = {
    val a = "8478231212478987482988429808779810457634781384756794987"
    val aScale = 41
    val b = "92948782094488478231212478987482988429808779810457634781384756794987"
    val bScale = -24
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    assertTrue(aNumber.hashCode != bNumber.hashCode)
  }

  @Test def testMaxEqual(): Unit = {
    val a = "8478231212478987482988429808779810457634781384756794987"
    val aScale = 41
    val b = "8478231212478987482988429808779810457634781384756794987"
    val bScale = 41
    val c = "8478231212478987482988429808779810457634781384756794987"
    val cScale = 41
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val cNumber = new BigDecimal(new BigInteger(c), cScale)
    assertTrue(cNumber == aNumber.max(bNumber))
  }

  @Test def testMaxUnequal1(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = 24
    val b = "92948782094488478231212478987482988429808779810457634781384756794987"
    val bScale = 41
    val c = "92948782094488478231212478987482988429808779810457634781384756794987"
    val cScale = 24
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val cNumber = new BigDecimal(new BigInteger(c), cScale)
    assertTrue(cNumber == aNumber.max(bNumber))
  }

  @Test def testMaxUnequal2(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = 41
    val b = "94488478231212478987482988429808779810457634781384756794987"
    val bScale = 41
    val c = "92948782094488478231212478987482988429808779810457634781384756794987"
    val cScale = 41
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val cNumber = new BigDecimal(new BigInteger(c), cScale)
    assertTrue(cNumber == aNumber.max(bNumber))
  }

  @Test def testMinEqual(): Unit = {
    val a = "8478231212478987482988429808779810457634781384756794987"
    val aScale = 41
    val b = "8478231212478987482988429808779810457634781384756794987"
    val bScale = 41
    val c = "8478231212478987482988429808779810457634781384756794987"
    val cScale = 41
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val cNumber = new BigDecimal(new BigInteger(c), cScale)
    assertTrue(cNumber == aNumber.min(bNumber))
  }

  @Test def testMinUnequal1(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = 24
    val b = "92948782094488478231212478987482988429808779810457634781384756794987"
    val bScale = 41
    val c = "92948782094488478231212478987482988429808779810457634781384756794987"
    val cScale = 41
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val cNumber = new BigDecimal(new BigInteger(c), cScale)
    assertTrue(cNumber == aNumber.min(bNumber))
  }

  @Test def testMinUnequal2(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = 41
    val b = "94488478231212478987482988429808779810457634781384756794987"
    val bScale = 41
    val c = "94488478231212478987482988429808779810457634781384756794987"
    val cScale = 41
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val cNumber = new BigDecimal(new BigInteger(c), cScale)
    assertTrue(cNumber == aNumber.min(bNumber))
  }

  @Test def testNegateMathContextNegative(): Unit = {
    val a = "-92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = 49
    val precision = 46
    val rm = RoundingMode.CEILING
    val mc = new MathContext(precision, rm)
    val c = "9294878209448847823.121247898748298842980877982"
    val cScale = 27
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val res = aNumber.negate(mc)
    assertEquals(res.toString, c)
    assertEquals(res.scale(), cScale)
  }

  @Test def testNegateMathContextPositive(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = 41
    val precision = 37
    val rm = RoundingMode.FLOOR
    val mc = new MathContext(precision, rm)
    val c = "-929487820944884782312124789.8748298843"
    val cScale = 10
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val res = aNumber.negate(mc)
    assertEquals(res.toString, c)
    assertEquals(res.scale(), cScale)
  }

  @Test def testNegateNegative(): Unit = {
    val a = "-92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = 41
    val c = "92948782094488478231212478987482988429808779810457634781384756794987"
    val cScale = 41
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val cNumber = new BigDecimal(new BigInteger(c), cScale)
    assertTrue(cNumber == aNumber.negate())
  }

  @Test def testNegatePositive(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = 41
    val c = "-92948782094488478231212478987482988429808779810457634781384756794987"
    val cScale = 41
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val cNumber = new BigDecimal(new BigInteger(c), cScale)
    assertTrue(cNumber == aNumber.negate())
  }

  @Test def testPlusMathContextNegative(): Unit = {
    val a = "-92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = 49
    val precision = 46
    val rm = RoundingMode.CEILING
    val mc = new MathContext(precision, rm)
    val c = "-9294878209448847823.121247898748298842980877981"
    val cScale = 27
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val res = aNumber.plus(mc)
    assertEquals(res.toString, c)
    assertEquals(res.scale(), cScale)
  }

  @Test def testPlusMathContextPositive(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = 41
    val precision = 37
    val rm = RoundingMode.FLOOR
    val mc = new MathContext(precision, rm)
    val c = "929487820944884782312124789.8748298842"
    val cScale = 10
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val res = aNumber.plus(mc)
    assertEquals(res.toString, c)
    assertEquals(res.scale(), cScale)
  }

  @Test def testPlusNegative(): Unit = {
    val a = "-92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = 41
    val c = "-92948782094488478231212478987482988429808779810457634781384756794987"
    val cScale = 41
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val cNumber = new BigDecimal(new BigInteger(c), cScale)
    assertTrue(cNumber == aNumber.plus())
  }

  @Test def testPlusPositive(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = 41
    val c = "92948782094488478231212478987482988429808779810457634781384756794987"
    val cScale = 41
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val cNumber = new BigDecimal(new BigInteger(c), cScale)
    assertTrue(cNumber == aNumber.plus())
  }

  @Test def testSignumNegative(): Unit = {
    val a = "-92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = 41
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    assertEquals(-1, aNumber.signum())
  }

  @Test def testSignumPositive(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = 41
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    assertEquals(1, aNumber.signum())
  }

  @Test def testSignumZero(): Unit = {
    val a = "0"
    val aScale = 41
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    assertEquals(0, aNumber.signum())
  }
}
