// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigDecimalArithmeticTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math._

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform.executingInJVM

class  BigDecimalArithmeticTest {

  @Test def testAddDiffScaleNegPos(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = -15
    val b = "747233429293018787918347987234564568"
    val bScale = 10
    val c = "1231212478987482988429808779810457634781459480137916301878791834798.7234564568"
    val cScale = 10
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.add(bNumber)
    assertEquals(result.scale(), cScale)
    assertEquals(result.toString, c)
  }

  @Test def testAddDiffScalePosNeg(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 15
    val b = "747233429293018787918347987234564568"
    val bScale = -10
    val c = "7472334294161400358170962860775454459810457634.781384756794987"
    val cScale = 15
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.add(bNumber)
    assertEquals(result.scale(), cScale)
    assertEquals(result.toString, c)
  }

  @Test def testAddDiffScalePosPos(): Unit = {
    val a = "100"
    val aScale = 15
    val b = "200"
    val bScale = 14
    val c = "2.100E-12"
    val cScale = 15
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.add(bNumber)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testAddDiffScaleZeroZero(): Unit = {
    val a = "0"
    val aScale = -15
    val b = "0"
    val bScale = 10
    val c = "0E-10"
    val cScale = 10
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.add(bNumber)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testAddEqualScaleNegNeg(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = -10
    val b = "747233429293018787918347987234564568"
    val bScale = -10
    val c = "1.231212478987483735663238072829245553129371991359555E+61"
    val cScale = -10
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.add(bNumber)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testAddEqualScalePosPos(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 10
    val b = "747233429293018787918347987234564568"
    val bScale = 10
    val c = "123121247898748373566323807282924555312937.1991359555"
    val cScale = 10
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.add(bNumber)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testAddMathContextDiffScalePosNeg(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 15
    val b = "747233429293018787918347987234564568"
    val bScale = -10
    val c = "7.47233429416141E+45"
    val cScale = -31
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val mc = new MathContext(15, RoundingMode.CEILING)
    val result = aNumber.add(bNumber, mc)
    assertEquals(c.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testAddMathContextEqualScaleNegNeg(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = -10
    val b = "747233429293018787918347987234564568"
    val bScale = -10
    val c = "1.2312E+61"
    val cScale = -57
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val mc = new MathContext(5, RoundingMode.FLOOR)
    val result = aNumber.add(bNumber, mc)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testAddMathContextEqualScalePosPos(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 10
    val b = "747233429293018787918347987234564568"
    val bScale = 10
    val c = "1.2313E+41"
    val cScale = -37
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val mc = new MathContext(5, RoundingMode.UP)
    val result = aNumber.add(bNumber, mc)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testAddZero(): Unit = {
    var bd = new BigDecimal("123456789")
    var sum = bd.add(BigDecimal.ZERO)
    assertTrue(sum == bd)
    sum = BigDecimal.ZERO.add(bd)
    assertTrue(sum == bd)
    bd = BigDecimal.valueOf(0L, 1)
    sum = bd.add(BigDecimal.ZERO)
    assertTrue(sum == bd)
  }

  @Test def testApproxScale(): Unit = {
    val decVal = BigDecimal.TEN.multiply(new BigDecimal("0.1"))
    val compare = decVal.compareTo(new BigDecimal("1.00"))
    assertEquals(compare, 0)
  }

  @Test def testDivideAndRemainder1(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = 45
    val b = "134432345432345748766876876723342238476237823787879183470"
    val bScale = 70
    val res = "277923185514690367474770683"
    val resScale = 0
    val rem = "1.3032693871288309587558885943391070087960319452465789990E-15"
    val remScale = 70
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divideAndRemainder(bNumber)
    assertEquals(result(0).toString, res)
    assertEquals(result(0).scale(), resScale)
    assertEquals(result(1).toString, rem)
    assertEquals(result(1).scale(), remScale)
  }

  @Test def testDivideAndRemainder2(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = -45
    val b = "134432345432345748766876876723342238476237823787879183470"
    val bScale = 70
    val res = "2779231855146903674747706830969461168692256919247547952" +
      "2608549363170374005512836303475980101168105698072946555" +
      "6862849"
    val resScale = 0
    val rem = "3.4935796954060524114470681810486417234751682675102093970E-15"
    val remScale = 70
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divideAndRemainder(bNumber)
    assertEquals(result(0).toString, res)
    assertEquals(result(0).scale(), resScale)
    assertEquals(result(1).toString, rem)
    assertEquals(result(1).scale(), remScale)
  }

  @Test def testDivideAndRemainderMathContextDOWN(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = 45
    val b = "134432345432345748766876876723342238476237823787879183470"
    val bScale = 20
    val precision = 15
    val rm = RoundingMode.DOWN
    val mc = new MathContext(precision, rm)
    val res = "0E-25"
    val resScale = 25
    val rem = "3736186567876.876578956958765675671119238118911893939591735"
    val remScale = 45
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divideAndRemainder(bNumber, mc)
    assertEquals(result(0).toString, res)
    assertEquals(result(0).scale(), resScale)
    assertEquals(result(1).toString, rem)
    assertEquals(result(1).scale(), remScale)
  }

  @Test def testDivideAndRemainderMathContextUP(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = 45
    val b = "134432345432345748766876876723342238476237823787879183470"
    val bScale = 70
    val precision = 75
    val rm = RoundingMode.UP
    val mc = new MathContext(precision, rm)
    val res = "277923185514690367474770683"
    val resScale = 0
    val rem = "1.3032693871288309587558885943391070087960319452465789990E-15"
    val remScale = 70
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divideAndRemainder(bNumber, mc)
    assertEquals(result(0).toString, res)
    assertEquals(result(0).scale(), resScale)
    assertEquals(result(1).toString, rem)
    assertEquals(result(1).scale(), remScale)
  }

  @Test def testDivideBigDecimal1(): Unit = {
    val a = "-37361671119238118911893939591735"
    val aScale = 10
    val b = "74723342238476237823787879183470"
    val bScale = 15
    val c = "-5E+4"
    val resScale = -4
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideBigDecimal2(): Unit = {
    val a = "-37361671119238118911893939591735"
    val aScale = 10
    val b = "74723342238476237823787879183470"
    val bScale = -15
    val c = "-5E-26"
    val resScale = 26
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideBigDecimalScaleMathContextCEILING(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = 15
    val b = "748766876876723342238476237823787879183470"
    val bScale = 70
    val precision = 21
    val rm = RoundingMode.CEILING
    val mc = new MathContext(precision, rm)
    val c = "4.98978611802562512996E+70"
    val resScale = -50
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, mc)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideBigDecimalScaleMathContextDOWN(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = 15
    val b = "748766876876723342238476237823787879183470"
    val bScale = 70
    val precision = 21
    val rm = RoundingMode.DOWN
    val mc = new MathContext(precision, rm)
    val c = "4.98978611802562512995E+70"
    val resScale = -50
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, mc)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideBigDecimalScaleMathContextFLOOR(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = 15
    val b = "748766876876723342238476237823787879183470"
    val bScale = 70
    val precision = 21
    val rm = RoundingMode.FLOOR
    val mc = new MathContext(precision, rm)
    val c = "4.98978611802562512995E+70"
    val resScale = -50
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, mc)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideBigDecimalScaleMathContextHALF_DOWN(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = 45
    val b = "134432345432345748766876876723342238476237823787879183470"
    val bScale = 70
    val precision = 21
    val rm = RoundingMode.HALF_DOWN
    val mc = new MathContext(precision, rm)
    val c = "2.77923185514690367475E+26"
    val resScale = -6
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, mc)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideBigDecimalScaleMathContextHALF_EVEN(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = 45
    val b = "134432345432345748766876876723342238476237823787879183470"
    val bScale = 70
    val precision = 21
    val rm = RoundingMode.HALF_EVEN
    val mc = new MathContext(precision, rm)
    val c = "2.77923185514690367475E+26"
    val resScale = -6
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, mc)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideBigDecimalScaleMathContextHALF_UP(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = 45
    val b = "134432345432345748766876876723342238476237823787879183470"
    val bScale = 70
    val precision = 21
    val rm = RoundingMode.HALF_UP
    val mc = new MathContext(precision, rm)
    val c = "2.77923185514690367475E+26"
    val resScale = -6
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, mc)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideBigDecimalScaleMathContextUP(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = 15
    val b = "748766876876723342238476237823787879183470"
    val bScale = 10
    val precision = 21
    val rm = RoundingMode.UP
    val mc = new MathContext(precision, rm)
    val c = "49897861180.2562512996"
    val resScale = 10
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, mc)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideBigDecimalScaleRoundingModeCEILING(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = 100
    val b = "74723342238476237823787879183470"
    val bScale = 15
    val newScale = 45
    val rm = RoundingMode.CEILING
    val c = "1E-45"
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, newScale, rm)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), newScale)
  }

  @Test def testDivideBigDecimalScaleRoundingModeDOWN(): Unit = {
    val a = "-37361671119238118911893939591735"
    val aScale = 10
    val b = "74723342238476237823787879183470"
    val bScale = 15
    val newScale = 31
    val rm = RoundingMode.DOWN
    val c = "-50000.0000000000000000000000000000000"
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, newScale, rm)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), newScale)
  }

  @Test def testDivideBigDecimalScaleRoundingModeFLOOR(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = 100
    val b = "74723342238476237823787879183470"
    val bScale = 15
    val newScale = 45
    val rm = RoundingMode.FLOOR
    val c = "0E-45"
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, newScale, rm)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), newScale)
  }

  @Test def testDivideBigDecimalScaleRoundingModeHALF_DOWN(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = 5
    val b = "74723342238476237823787879183470"
    val bScale = 15
    val newScale = 7
    val rm = RoundingMode.HALF_DOWN
    val c = "500002603731642864013619132621009722.1803810"
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, newScale, rm)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), newScale)
  }

  @Test def testDivideBigDecimalScaleRoundingModeHALF_EVEN(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = 5
    val b = "74723342238476237823787879183470"
    val bScale = 15
    val newScale = 7
    val rm = RoundingMode.HALF_EVEN
    val c = "500002603731642864013619132621009722.1803810"
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, newScale, rm)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), newScale)
  }

  @Test def testDivideBigDecimalScaleRoundingModeHALF_UP(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = -51
    val b = "74723342238476237823787879183470"
    val bScale = 45
    val newScale = 3
    val rm = RoundingMode.HALF_UP
    val c = "50000260373164286401361913262100972218038099522752460421" +
      "05959924024355721031761947728703598332749334086415670525" +
      "3761096961.670"
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, newScale, rm)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), newScale)
  }

  @Test def testDivideBigDecimalScaleRoundingModeUP(): Unit = {
    val a = "-37361671119238118911893939591735"
    val aScale = 10
    val b = "74723342238476237823787879183470"
    val bScale = -15
    val newScale = 31
    val rm = RoundingMode.UP
    val c = "-5.00000E-26"
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, newScale, rm)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), newScale)
  }

  @Test def testDivideBigDecimalScale_issue2755(): Unit = {
    val a = new BigDecimal(2L)
    val b = new BigDecimal(1L)
    val r = a.divide(b, 1, RoundingMode.UNNECESSARY)
    assertEquals(1, r.scale())
    assertEquals(2L, r.longValueExact())
  }

  @Test def testDivideByZero(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 15
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = BigDecimal.valueOf(0L)
    expectThrows(classOf[ArithmeticException], aNumber.divide(bNumber))
  }

  @Test def testDivideExceptionInvalidRM(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 15
    val b = "747233429293018787918347987234564568"
    val bScale = 10
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    expectThrows(classOf[IllegalArgumentException], aNumber.divide(bNumber, 100))
  }

  @Test def testDivideExceptionRM(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 15
    val b = "747233429293018787918347987234564568"
    val bScale = 10
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    expectThrows(classOf[ArithmeticException],
        aNumber.divide(bNumber, BigDecimal.ROUND_UNNECESSARY))
  }

  @Test def testDivideExpEqualsZero(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = -15
    val b = "747233429293018787918347987234564568"
    val bScale = 10
    val c = "1.64769459009933764189139568605273529E+40"
    val resScale = -5
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_CEILING)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideExpGreaterZero(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = -15
    val b = "747233429293018787918347987234564568"
    val bScale = 20
    val c = "1.647694590099337641891395686052735285121058381E+50"
    val resScale = -5
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_CEILING)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideExpLessZero(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 15
    val b = "747233429293018787918347987234564568"
    val bScale = 10
    val c = "1.64770E+10"
    val resScale = -5
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_CEILING)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideLargeScale(): Unit = {
    val arg1 = new BigDecimal("320.0E+2147483647")
    val arg2 = new BigDecimal("6E-2147483647")
    expectThrows(classOf[ArithmeticException],
        arg1.divide(arg2, Int.MaxValue, RoundingMode.CEILING))
  }

  @Test def testDivideRemainderIsZero(): Unit = {
    val a = "8311389578904553209874735431110"
    val aScale = -15
    val b = "237468273682987234567849583746"
    val bScale = 20
    val c = "3.5000000000000000000000000000000E+36"
    val resScale = -5
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_CEILING)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundCeilingNeg(): Unit = {
    val a = "-92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = -24
    val b = "7472334223847623782375469293018787918347987234564568"
    val bScale = 13
    val c = "-1.24390557635720517122423359799283E+53"
    val resScale = -21
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_CEILING)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundCeilingPos(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = -24
    val b = "7472334223847623782375469293018787918347987234564568"
    val bScale = 13
    val c = "1.24390557635720517122423359799284E+53"
    val resScale = -21
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_CEILING)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundDownNeg(): Unit = {
    val a = "-92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = -24
    val b = "7472334223847623782375469293018787918347987234564568"
    val bScale = 13
    val c = "-1.24390557635720517122423359799283E+53"
    val resScale = -21
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_DOWN)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundDownPos(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = -24
    val b = "7472334223847623782375469293018787918347987234564568"
    val bScale = 13
    val c = "1.24390557635720517122423359799283E+53"
    val resScale = -21
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_DOWN)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundFloorNeg(): Unit = {
    val a = "-92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = -24
    val b = "7472334223847623782375469293018787918347987234564568"
    val bScale = 13
    val c = "-1.24390557635720517122423359799284E+53"
    val resScale = -21
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_FLOOR)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundFloorPos(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = -24
    val b = "7472334223847623782375469293018787918347987234564568"
    val bScale = 13
    val c = "1.24390557635720517122423359799283E+53"
    val resScale = -21
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_FLOOR)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundHalfDownNeg(): Unit = {
    val a = "-92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = -24
    val b = "7472334223847623782375469293018787918347987234564568"
    val bScale = 13
    val c = "-1.24390557635720517122423359799284E+53"
    val resScale = -21
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_HALF_DOWN)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundHalfDownNeg1(): Unit = {
    val a = "-92948782094488478231212478987482988798104576347813847567949855464535634534563456"
    val aScale = -24
    val b = "74723342238476237823754692930187879183479"
    val bScale = 13
    val c = "-1.2439055763572051712242335979928354832010167729111113605E+76"
    val resScale = -21
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_HALF_DOWN)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundHalfDownNeg2(): Unit = {
    val a = "-37361671119238118911893939591735"
    val aScale = 10
    val b = "74723342238476237823787879183470"
    val bScale = 15
    val c = "0E+5"
    val resScale = -5
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_HALF_DOWN)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundHalfDownPos(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = -24
    val b = "7472334223847623782375469293018787918347987234564568"
    val bScale = 13
    val c = "1.24390557635720517122423359799284E+53"
    val resScale = -21
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_HALF_DOWN)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundHalfDownPos1(): Unit = {
    val a = "92948782094488478231212478987482988798104576347813847567949855464535634534563456"
    val aScale = -24
    val b = "74723342238476237823754692930187879183479"
    val bScale = 13
    val c = "1.2439055763572051712242335979928354832010167729111113605E+76"
    val resScale = -21
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_HALF_DOWN)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundHalfEvenNeg(): Unit = {
    val a = "-92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = -24
    val b = "7472334223847623782375469293018787918347987234564568"
    val bScale = 13
    val c = "-1.24390557635720517122423359799284E+53"
    val resScale = -21
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_HALF_EVEN)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundHalfEvenNeg1(): Unit = {
    val a = "-92948782094488478231212478987482988798104576347813847567949855464535634534563456"
    val aScale = -24
    val b = "74723342238476237823754692930187879183479"
    val bScale = 13
    val c = "-1.2439055763572051712242335979928354832010167729111113605E+76"
    val resScale = -21
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_HALF_EVEN)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundHalfEvenNeg2(): Unit = {
    val a = "-37361671119238118911893939591735"
    val aScale = 10
    val b = "74723342238476237823787879183470"
    val bScale = 15
    val c = "0E+5"
    val resScale = -5
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_HALF_EVEN)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundHalfEvenPos(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = -24
    val b = "7472334223847623782375469293018787918347987234564568"
    val bScale = 13
    val c = "1.24390557635720517122423359799284E+53"
    val resScale = -21
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_HALF_EVEN)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundHalfEvenPos1(): Unit = {
    val a = "92948782094488478231212478987482988798104576347813847567949855464535634534563456"
    val aScale = -24
    val b = "74723342238476237823754692930187879183479"
    val bScale = 13
    val c = "1.2439055763572051712242335979928354832010167729111113605E+76"
    val resScale = -21
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_HALF_EVEN)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundHalfUpNeg(): Unit = {
    val a = "-92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = -24
    val b = "7472334223847623782375469293018787918347987234564568"
    val bScale = 13
    val c = "-1.24390557635720517122423359799284E+53"
    val resScale = -21
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_HALF_UP)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundHalfUpNeg1(): Unit = {
    val a = "-92948782094488478231212478987482988798104576347813847567949855464535634534563456"
    val aScale = -24
    val b = "74723342238476237823754692930187879183479"
    val bScale = 13
    val c = "-1.2439055763572051712242335979928354832010167729111113605E+76"
    val resScale = -21
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_HALF_UP)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundHalfUpNeg2(): Unit = {
    val a = "-37361671119238118911893939591735"
    val aScale = 10
    val b = "74723342238476237823787879183470"
    val bScale = 15
    val c = "-1E+5"
    val resScale = -5
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_HALF_UP)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundHalfUpPos(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = -24
    val b = "7472334223847623782375469293018787918347987234564568"
    val bScale = 13
    val c = "1.24390557635720517122423359799284E+53"
    val resScale = -21
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_HALF_UP)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundHalfUpPos1(): Unit = {
    val a = "92948782094488478231212478987482988798104576347813847567949855464535634534563456"
    val aScale = -24
    val b = "74723342238476237823754692930187879183479"
    val bScale = 13
    val c = "1.2439055763572051712242335979928354832010167729111113605E+76"
    val resScale = -21
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_HALF_UP)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundUpNeg(): Unit = {
    val a = "-92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = -24
    val b = "7472334223847623782375469293018787918347987234564568"
    val bScale = 13
    val c = "-1.24390557635720517122423359799284E+53"
    val resScale = -21
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_UP)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideRoundUpPos(): Unit = {
    val a = "92948782094488478231212478987482988429808779810457634781384756794987"
    val aScale = -24
    val b = "7472334223847623782375469293018787918347987234564568"
    val bScale = 13
    val c = "1.24390557635720517122423359799284E+53"
    val resScale = -21
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divide(bNumber, resScale, BigDecimal.ROUND_UP)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideSmall(): Unit = {
    val a = BigDecimal.valueOf(6)
    val b = BigDecimal.valueOf(2)
    var quotient = a.divide(b)
    assertEquals(quotient.toString, "3")
    quotient = a.divideToIntegralValue(b)
    assertEquals(quotient.toString, "3")
    quotient = a.divide(BigDecimal.ONE)
    assertEquals(quotient.toString, "6")
    quotient = a.divide(BigDecimal.ONE.negate())
    assertEquals(quotient.toString, "-6")
  }

  @Test def testDivideToIntegralValue(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = 45
    val b = "134432345432345748766876876723342238476237823787879183470"
    val bScale = 70
    val c = "277923185514690367474770683"
    val resScale = 0
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divideToIntegralValue(bNumber)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideToIntegralValueMathContextDOWN(): Unit = {
    val a = "3736186567876876578956958769675785435673453453653543654354365435675671119238118911893939591735"
    val aScale = 45
    val b = "134432345432345748766876876723342238476237823787879183470"
    val bScale = 70
    val precision = 75
    val rm = RoundingMode.DOWN
    val mc = new MathContext(precision, rm)
    val c = "2.7792318551469036747477068339450205874992634417590178670822889E+62"
    val resScale = -1
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divideToIntegralValue(bNumber, mc)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideToIntegralValueMathContextUP(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = 45
    val b = "134432345432345748766876876723342238476237823787879183470"
    val bScale = 70
    val precision = 32
    val rm = RoundingMode.UP
    val mc = new MathContext(precision, rm)
    val c = "277923185514690367474770683"
    val resScale = 0
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.divideToIntegralValue(bNumber, mc)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), resScale)
  }

  @Test def testDivideZero(): Unit = {
    var quotient = BigDecimal.ZERO.divide(BigDecimal.ONE)
    assertTrue(BigDecimal.ZERO == quotient)
    quotient = BigDecimal.ZERO.negate().divide(BigDecimal.ONE)
    assertTrue(BigDecimal.ZERO == quotient)
    expectThrows(classOf[ArithmeticException], BigDecimal.ZERO.divide(BigDecimal.ZERO))
    expectThrows(classOf[ArithmeticException], BigDecimal.ONE.divide(BigDecimal.ZERO))
    expectThrows(classOf[ArithmeticException], BigDecimal.ONE.divideToIntegralValue(BigDecimal.ZERO))
  }

  @Test def testDivideToIntegralValue_on_floating_points__issue_1979(): Unit = {
    val one = new BigDecimal(1.0)
    val oneAndHalf = new BigDecimal(1.5)
    val a0 = new BigDecimal(3.0)
    val a1 = new BigDecimal(3.1)
    val a2 = new BigDecimal(3.21)
    val a3 = new BigDecimal(3.321)
    val b0 = new BigDecimal(3.0)
    val b1 = new BigDecimal(2.0)

    assertEquals(b0, a0.divideToIntegralValue(one))
    assertEquals(b0.setScale(51), a1.divideToIntegralValue(one))
    assertEquals(b0.setScale(50), a2.divideToIntegralValue(one))
    assertEquals(b0.setScale(51), a3.divideToIntegralValue(one))

    assertEquals(b1, a0.divideToIntegralValue(oneAndHalf))
    assertEquals(b1.setScale(50), a1.divideToIntegralValue(oneAndHalf))
    assertEquals(b1.setScale(49), a2.divideToIntegralValue(oneAndHalf))
    assertEquals(b1.setScale(50), a3.divideToIntegralValue(oneAndHalf))
  }

  @Test def testMultiplyDiffScaleNegPos(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = -15
    val b = "747233429293018787918347987234564568"
    val bScale = 10
    val c = "9.20003122862175749786430095741145455670101391569026662845893091880727173060570190220616E+91"
    val cScale = -5
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.multiply(bNumber)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testMultiplyDiffScalePosNeg(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 10
    val b = "747233429293018787918347987234564568"
    val bScale = -10
    val c = "920003122862175749786430095741145455670101391569026662845893091880727173060570190220616"
    val cScale = 0
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.multiply(bNumber)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testMultiplyEqualScaleNegNeg(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = -15
    val b = "747233429293018787918347987234564568"
    val bScale = -10
    val c = "9.20003122862175749786430095741145455670101391569026662845893091880727173060570190220616E+111"
    val cScale = -25
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.multiply(bNumber)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testMultiplyMathContextDiffScaleNegPos(): Unit = {
    val a = "488757458676796558668876576576579097029810457634781384756794987"
    val aScale = -63
    val b = "747233429293018787918347987234564568"
    val bScale = 63
    val c = "3.6521591193960361339707130098174381429788164316E+98"
    val cScale = -52
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val mc = new MathContext(47, RoundingMode.HALF_UP)
    val result = aNumber.multiply(bNumber, mc)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testMultiplyMathContextDiffScalePosNeg(): Unit = {
    val a = "987667796597975765768768767866756808779810457634781384756794987"
    val aScale = 100
    val b = "747233429293018787918347987234564568"
    val bScale = -70
    val c = "7.3801839465418518653942222612429081498248509257207477E+68"
    val cScale = -16
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val mc = new MathContext(53, RoundingMode.HALF_UP)
    val result = aNumber.multiply(bNumber, mc)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testMultiplyMathContextScalePosPos(): Unit = {
    val a = "97665696756578755423325476545428779810457634781384756794987"
    val aScale = -25
    val b = "87656965586786097685674786576598865"
    val bScale = 10
    val c = "8.561078619600910561431314228543672720908E+108"
    val cScale = -69
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val mc = new MathContext(40, RoundingMode.HALF_DOWN)
    val result = aNumber.multiply(bNumber, mc)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testMultiplyScalePosPos(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 15
    val b = "747233429293018787918347987234564568"
    val bScale = 10
    val c = "92000312286217574978643009574114545567010139156902666284589309.1880727173060570190220616"
    val cScale = 25
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.multiply(bNumber)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testMultiplySmallOverflow_issue2587(): Unit = {
    val x = new BigDecimal(Int.MinValue)
    val y = new BigDecimal(Int.MinValue.toLong * 2L)
    val z = new BigDecimal("9223372036854775808")
    assertEquals(z, x.multiply(y))
    assertEquals(z, y.multiply(x))
  }

  @Test def testPow(): Unit = {
    val a = "123121247898748298842980"
    val aScale = 10
    val exp = 10
    val c = "8004424019039195734129783677098845174704975003788210729597" +
      "4875206425711159855030832837132149513512555214958035390490" +
      "798520842025826.594316163502809818340013610490541783276343" +
      "6514490899700151256484355936102754469438371850240000000000"
    val cScale = 100
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val result = aNumber.pow(exp)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testPow0(): Unit = {
    val a = "123121247898748298842980"
    val aScale = 10
    val exp = 0
    val c = "1"
    val cScale = 0
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val result = aNumber.pow(exp)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testPowMathContext(): Unit = {
    val a = "123121247898748298842980"
    val aScale = 10
    val exp = 10
    val c = "8.0044E+130"
    val cScale = -126
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val mc = new MathContext(5, RoundingMode.HALF_UP)
    val result = aNumber.pow(exp, mc)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testRemainder1(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = 45
    val b = "134432345432345748766876876723342238476237823787879183470"
    val bScale = 10
    val res = "3736186567876.876578956958765675671119238118911893939591735"
    val resScale = 45
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.remainder(bNumber)
    assertEquals(result.toString, res)
    assertEquals(result.scale(), resScale)
  }

  @Test def testRemainder2(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = -45
    val b = "134432345432345748766876876723342238476237823787879183470"
    val bScale = 10
    val res = "1149310942946292909508821656680979993738625937.2065885780"
    val resScale = 10
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.remainder(bNumber)
    assertEquals(result.toString, res)
    assertEquals(result.scale(), resScale)
  }

  @Test def testRemainderMathContextHALF_DOWN(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = -45
    val b = "134432345432345748766876876723342238476237823787879183470"
    val bScale = 10
    val precision = 75
    val rm = RoundingMode.HALF_DOWN
    val mc = new MathContext(precision, rm)
    val res = "1149310942946292909508821656680979993738625937.2065885780"
    val resScale = 10
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.remainder(bNumber, mc)
    assertEquals(result.toString, res)
    assertEquals(result.scale(), resScale)
  }

  @Test def testRemainderMathContextHALF_UP(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = 45
    val b = "134432345432345748766876876723342238476237823787879183470"
    val bScale = 10
    val precision = 15
    val rm = RoundingMode.HALF_UP
    val mc = new MathContext(precision, rm)
    val res = "3736186567876.876578956958765675671119238118911893939591735"
    val resScale = 45
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.remainder(bNumber, mc)
    assertEquals(result.toString, res)
    assertEquals(result.scale(), resScale)
  }

  @Test def testRoundMathContextCEILING(): Unit = {
    var `val` = BigDecimal.valueOf(1.5)
    val result = `val`.round(new MathContext(1, RoundingMode.CEILING))
    assertEquals(result.toString, "2")
    assertEquals(result.scale(), 0)
    assertEquals(result.precision(), 1)
    `val` = BigDecimal.valueOf(5.43445663479765)
    `val` = `val`.setScale(`val`.scale() + 1, RoundingMode.CEILING)
      .round(new MathContext(1, RoundingMode.CEILING))
    `val` = BigDecimal.valueOf(5.4344566347976)
    `val` = `val`.setScale(`val`.scale() + 2, RoundingMode.CEILING)
      .round(new MathContext(1, RoundingMode.CEILING))
    var test = BigDecimal.valueOf(12.4344566347976)
    test = test.setScale(test.scale() + 1, RoundingMode.CEILING)
      .round(new MathContext(1, RoundingMode.CEILING))
  }

  @Test def testRoundMathContextHALF_DOWN(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = -45
    val precision = 75
    val rm = RoundingMode.HALF_DOWN
    val mc = new MathContext(precision, rm)
    val res = "3.736186567876876578956958765675671119238118911893939591735E+102"
    val resScale = -45
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val result = aNumber.round(mc)
    assertEquals(result.toString, res)
    assertEquals(result.scale(), resScale)
  }

  @Test def testRoundMathContextHALF_UP(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = 45
    val precision = 15
    val rm = RoundingMode.HALF_UP
    val mc = new MathContext(precision, rm)
    val res = "3736186567876.88"
    val resScale = 2
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val result = aNumber.round(mc)
    assertEquals(result.toString, res)
    assertEquals(result.scale(), resScale)
  }

  @Test def testRoundMathContextPrecision0(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = 45
    val precision = 0
    val rm = RoundingMode.HALF_UP
    val mc = new MathContext(precision, rm)
    val res = "3736186567876.876578956958765675671119238118911893939591735"
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val result = aNumber.round(mc)
    assertEquals(result.toString, res)
    assertEquals(result.scale(), aScale)
  }

  @Test def testSubtractDiffScaleNegPos(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = -15
    val b = "747233429293018787918347987234564568"
    val bScale = 10
    val c = "1231212478987482988429808779810457634781310033452057698121208165201.2765435432"
    val cScale = 10
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.subtract(bNumber)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testSubtractDiffScalePosNeg(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 15
    val b = "747233429293018787918347987234564568"
    val bScale = -10
    val c = "-7472334291698975400195996883915836900189542365.218615243205013"
    val cScale = 15
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.subtract(bNumber)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testSubtractEqualScaleNegNeg(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = -10
    val b = "747233429293018787918347987234564568"
    val bScale = -10
    val c = "1.231212478987482241196379486791669716433397522230419E+61"
    val cScale = -10
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.subtract(bNumber)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testSubtractEqualScalePosPos(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 10
    val b = "747233429293018787918347987234564568"
    val bScale = 10
    val c = "123121247898748224119637948679166971643339.7522230419"
    val cScale = 10
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val result = aNumber.subtract(bNumber)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testSubtractMathContextDiffScaleNegPos(): Unit = {
    val a = "986798656676789766678767876078779810457634781384756794987"
    val aScale = -15
    val b = "747233429293018787918347987234564568"
    val bScale = 40
    val c = "9.867986566767897666787678760787798104576347813847567949870000000000000E+71"
    val cScale = -2
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val mc = new MathContext(70, RoundingMode.HALF_DOWN)
    val result = aNumber.subtract(bNumber, mc)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testSubtractMathContextDiffScalePosNeg(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 15
    val b = "747233429293018787918347987234564568"
    val bScale = -10
    val c = "-7.4723342916989754E+45"
    val cScale = -29
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val mc = new MathContext(17, RoundingMode.DOWN)
    val result = aNumber.subtract(bNumber, mc)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testSubtractMathContextEqualScalePosPos(): Unit = {
    val a = "1231212478987482988429808779810457634781384756794987"
    val aScale = 10
    val b = "747233429293018787918347987234564568"
    val bScale = 10
    val c = "1.23121247898749E+41"
    val cScale = -27
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val bNumber = new BigDecimal(new BigInteger(b), bScale)
    val mc = new MathContext(15, RoundingMode.CEILING)
    val result = aNumber.subtract(bNumber, mc)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }

  @Test def testUlpNeg(): Unit = {
    val a = "-3736186567876876578956958765675671119238118911893939591735"
    val aScale = 45
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val result = aNumber.ulp()
    val res = "1E-45"
    val resScale = 45
    assertEquals(result.toString, res)
    assertEquals(result.scale(), resScale)
  }

  @Test def testUlpPos(): Unit = {
    val a = "3736186567876876578956958765675671119238118911893939591735"
    val aScale = -45
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val result = aNumber.ulp()
    val res = "1E+45"
    val resScale = -45
    assertEquals(result.toString, res)
    assertEquals(result.scale(), resScale)
  }

  @Test def testUlpZero(): Unit = {
    val a = "0"
    val aScale = 2
    val aNumber = new BigDecimal(new BigInteger(a), aScale)
    val result = aNumber.ulp()
    val res = "0.01"
    val resScale = 2
    assertEquals(result.toString, res)
    assertEquals(result.scale(), resScale)
  }

  @Test def testZeroPow0(): Unit = {
    val c = "1"
    val cScale = 0
    val result = BigDecimal.ZERO.pow(0)
    assertEquals(result.toString, c)
    assertEquals(result.scale(), cScale)
  }
}
