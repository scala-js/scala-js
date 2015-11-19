// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/MathContextTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math.{MathContext, RoundingMode}

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

class MathContextTest {

  @Test def testMathContextSingleArgConstructor(): Unit = {
    val mc1 = new MathContext("precision=16 roundingMode=CEILING")
    assertTrue(mc1.getPrecision == 16)
    assertTrue(mc1.getRoundingMode == RoundingMode.CEILING)
    val mc2 = new MathContext("precision=17 roundingMode=DOWN")
    assertTrue(mc2.getPrecision == 17)
    assertTrue(mc2.getRoundingMode == RoundingMode.DOWN)
    val mc3 = new MathContext("precision=18 roundingMode=FLOOR")
    assertTrue(mc3.getPrecision == 18)
    assertTrue(mc3.getRoundingMode == RoundingMode.FLOOR)
    val mc4 = new MathContext("precision=19 roundingMode=HALF_DOWN")
    assertTrue(mc4.getPrecision == 19)
    assertTrue(mc4.getRoundingMode == RoundingMode.HALF_DOWN)
    val mc5 = new MathContext("precision=20 roundingMode=HALF_EVEN")
    assertTrue(mc5.getPrecision == 20)
    assertTrue(mc5.getRoundingMode == RoundingMode.HALF_EVEN)
    val mc6 = new MathContext("precision=21 roundingMode=HALF_UP")
    assertTrue(mc6.getPrecision == 21)
    assertTrue(mc6.getRoundingMode == RoundingMode.HALF_UP)
    val mc7 = new MathContext("precision=22 roundingMode=UNNECESSARY")
    assertTrue(mc7.getPrecision == 22)
    assertTrue(mc7.getRoundingMode == RoundingMode.UNNECESSARY)
    val mc8 = new MathContext("precision=23 roundingMode=UP")
    assertTrue(mc8.getPrecision == 23)
    assertTrue(mc8.getRoundingMode == RoundingMode.UP)

    expectThrows(classOf[IllegalArgumentException],
        new MathContext("prcision=27 roundingMode=CEILING"))
    expectThrows(classOf[IllegalArgumentException],
        new MathContext("precision=26 roundingMoe=CEILING"))
    expectThrows(classOf[IllegalArgumentException],
        new MathContext("precision=25 roundingMode=CEILINGFAN"))
    expectThrows(classOf[IllegalArgumentException],
        new MathContext("precision=24 roundingMode=HALF"))
    expectThrows(classOf[IllegalArgumentException],
        new MathContext("precision=23 roundingMode=UPSIDEDOWN"))
    expectThrows(classOf[IllegalArgumentException],
        new MathContext("precision=22roundingMode=UP"))
    expectThrows(classOf[IllegalArgumentException], new MathContext(""))
    expectThrows(classOf[NullPointerException], new MathContext(null))
  }

  @Test def testMathContextConstructorEquality(): Unit = {
    val mc1 = new MathContext(16, RoundingMode.CEILING)
    val mc1a = new MathContext("precision=16 roundingMode=CEILING")
    assertTrue(mc1 == mc1a)
    val mc2 = new MathContext(17, RoundingMode.DOWN)
    val mc2a = new MathContext("precision=17 roundingMode=DOWN")
    assertTrue(mc2 == mc2a)
    val mc3 = new MathContext(18, RoundingMode.FLOOR)
    val mc3a = new MathContext("precision=18 roundingMode=FLOOR")
    assertTrue(mc3 == mc3a)
    val mc4 = new MathContext(19, RoundingMode.HALF_DOWN)
    val mc4a = new MathContext("precision=19 roundingMode=HALF_DOWN")
    assertTrue(mc4 == mc4a)
    val mc5 = new MathContext(20, RoundingMode.HALF_EVEN)
    val mc5a = new MathContext("precision=20 roundingMode=HALF_EVEN")
    assertTrue(mc5 == mc5a)
    val mc6 = new MathContext(21, RoundingMode.HALF_UP)
    val mc6a = new MathContext("precision=21 roundingMode=HALF_UP")
    assertTrue(mc6 == mc6a)
    val mc7 = new MathContext(22, RoundingMode.UNNECESSARY)
    val mc7a = new MathContext("precision=22 roundingMode=UNNECESSARY")
    assertTrue(mc7 == mc7a)
    val mc8 = new MathContext(23, RoundingMode.UP)
    val mc8a = new MathContext("precision=23 roundingMode=UP")
    assertTrue(mc8 == mc8a)
  }
}
