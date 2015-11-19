/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.math

import java.math.BigDecimal

import org.junit.Test
import org.junit.Assert._

class BigDecimalTest  {

  // java.lang.Math.BigDecimal Int/Long Constructors

  @Test def `should accept 3 as aLong`(): Unit = {
    val bd = BigDecimal.valueOf(3L)
    assertEquals(3, bd.intValue())
    assertTrue(bd.longValue == 3L)
  }

  @Test def `should accept 999999999 as aLong`(): Unit = {
    val bd = BigDecimal.valueOf(999999999L)
    assertEquals(999999999, bd.intValue())
    assertTrue(bd.longValue == 999999999L)
  }

  @Test def `should accept 9999999999 as aLong`(): Unit = {
    val bd = BigDecimal.valueOf(9999999999L)
    assertTrue(bd.longValue == 9999999999L)
  }

  @Test def `should accept -999999999 as aLong`(): Unit = {
    val bd = BigDecimal.valueOf(-999999999L)
    assertEquals(-999999999, bd.intValue())
    assertTrue(bd.longValue == -999999999L)
  }

  @Test def `should accept -9999999999 as aLong`(): Unit = {
    val bd = BigDecimal.valueOf(-9999999999L)
    assertTrue(bd.longValue == -9999999999L)
  }

  @Test def `should accept 3 as a string`(): Unit = {
    val bd = new BigDecimal("3")
    assertEquals(3, bd.intValue())
    assertTrue(bd.longValue == 3L)
  }

  @Test def `should accept 99 as a string`(): Unit = {
    val bd = new BigDecimal("99")
    assertEquals(99, bd.intValue())
    assertTrue(bd.longValue == 99L)
  }

  @Test def `should accept 999999999 as string`(): Unit = {
    val bd = new BigDecimal("999999999")
    assertEquals(999999999, bd.intValue())
    assertTrue(bd.longValue == 999999999L)
  }

  @Test def `should accept 9999999999 as a string`(): Unit = {
    val bd = new BigDecimal("9999999999")
    assertTrue(bd.longValue == 9999999999L)
  }

  @Test def `should accept -99 as a string`(): Unit = {
    val bd = new BigDecimal("-99")
    assertEquals(-99, bd.intValue())
    assertTrue(bd.longValue == -99L)
  }

  @Test def `should accept -999999999 as sting`(): Unit = {
    val bd = new BigDecimal("-999999999")
    assertEquals(-999999999, bd.intValue())
    assertTrue(bd.longValue == -999999999L)
  }

  @Test def `should accept -9999999999 as a string`(): Unit = {
    val bd = new BigDecimal("-9999999999")
    assertTrue(bd.longValue == -9999999999L)
  }

  @Test def `should accept 9.9 as a string`(): Unit = {
    val bd = new BigDecimal("9.9")
    assertEquals("9.9", bd.toString)
    assertEquals(9.9, bd.doubleValue(), 0.0)
  }

  @Test def `should accept 99.99 as a string`(): Unit = {
    val bd = new BigDecimal("99.99")
    assertEquals(99.99, bd.doubleValue(), 0.0)
  }

  @Test def `should accept 999.999 as a string`(): Unit = {
    val bd = new BigDecimal("999.999")
    assertEquals(999.999, bd.doubleValue(), 0.0)
  }

  @Test def `should accept 9999.9999 as a string`(): Unit = {
    val bd = new BigDecimal("9999.9999")
    assertEquals(9999.9999, bd.doubleValue(), 0.0)
  }

  // java.lang.Math.BigDecimal double Constructors

  @Test def `should accept 3.3 as a double`(): Unit = {
    val d = 3.3
    val bd = new BigDecimal(d)
    assertEquals(d, bd.doubleValue(), 0.0)
  }

  @Test def `should accept 99.99 as a double`(): Unit = {
    val d = 99.99
    val bd = new BigDecimal(d)
    assertEquals(d, bd.doubleValue(), 0.0)
  }

  @Test def `should accept 9999.9999 as a double`(): Unit = {
    val d:Double = 9999.9999
    val bd = new BigDecimal(d)
    assertEquals(d, bd.doubleValue(), 0.0)
  }

  @Test def `should accept 99999999.99999999 as a double`(): Unit = {
    val d = 99999999.99999999
    val bd = new BigDecimal(d)
    assertEquals(d, bd.doubleValue(), 0.0)
  }

  @Test def `should accept 999999999.999999999 as a double`(): Unit = {
    val d = 999999999.999999999
    val bd = new BigDecimal(d)
    assertEquals(d, bd.doubleValue(), 0.0)
  }

  @Test def `should accept 9999999999.9999999999 as a double`(): Unit = {
    val d = 9999999999.9999999999
    val bd = new BigDecimal(d)
    assertEquals(d, bd.doubleValue(), 0.0)
  }

  @Test def `should accept -3.3 as a double`(): Unit = {
    val d = -3.3
    val bd = new BigDecimal(d)
    assertEquals(d, bd.doubleValue(), 0.0)
  }

  @Test def `should accept -99.99 as a double`(): Unit = {
    val d = -99.99
    val bd = new BigDecimal(d)
    assertEquals(d, bd.doubleValue(), 0.0)
  }

  @Test def `should accept -99999999.99999999 as a double`(): Unit = {
    val d = -99999999.99999999
    val bd = new BigDecimal(d)
    assertEquals(d, bd.doubleValue(), 0.0)
  }

  @Test def `should accept -999999999.999999999 as a double`(): Unit = {
    val d = -999999999.999999999
    val bd = new BigDecimal(d)
    assertEquals(d, bd.doubleValue(), 0.0)
  }

  @Test def `should accept -9999999999.9999999999 as a double`(): Unit = {
    val d = -9999999999.9999999999
    val bd = new BigDecimal(d)
    assertEquals(d, bd.doubleValue(), 0.0)
  }
}
