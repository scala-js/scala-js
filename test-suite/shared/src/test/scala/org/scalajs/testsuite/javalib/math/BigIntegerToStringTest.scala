// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerToStringTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math.BigInteger

import org.junit.Test
import org.junit.Assert._

class BigIntegerToStringTest {

  @Test def tes10PosVerySmall(): Unit = {
    val value = "2"
    val aNumber = new BigInteger(value)
    val result = aNumber.toString()
    assertEquals(2, aNumber.intValue())
    assertEquals(value, result)
  }

  @Test def tes10NegVerySmall(): Unit = {
    val value = "-2"
    val aNumber = new BigInteger(value)
    val result = aNumber.toString()
    assertEquals(-2, aNumber.intValue())
    assertEquals(value, result)
  }

  @Test def tes10PosSmall(): Unit = {
    val value = "24"
    val aNumber = new BigInteger(value)
    val result = aNumber.toString()
    assertEquals(24, aNumber.intValue())
    assertEquals(value, result)
  }

  @Test def tes10NegSmall(): Unit = {
    val value = "-24"
    val aNumber = new BigInteger(value)
    val result = aNumber.toString()
    assertEquals(-24, aNumber.intValue())
    assertEquals(value, result)
  }

  @Test def tes10PosLong(): Unit = {
    val value = "2489756308572"
    val aNumber = new BigInteger(value)
    val result = aNumber.toString()
    assertEquals(2489756308572L, aNumber.longValue())
    assertEquals(value, result)
  }

  @Test def tes10NegLong(): Unit = {
    val value = "-2489756308572"
    val aNumber = new BigInteger(value)
    val result = aNumber.toString()
    assertEquals(-2489756308572L, aNumber.longValue())
    assertEquals(value, result)
  }

  @Test def tes10Neg(): Unit = {
    val value = "-2489756308572364789878394872984"
    val aNumber = new BigInteger(value)
    val result = aNumber.toString()
    assertEquals(value, result)
  }

  @Test def testRadix10Neg(): Unit = {
    val value = "-2489756308572364789878394872984"
    val radix = 10
    val aNumber = new BigInteger(value, radix)
    val result = aNumber.toString(radix)
    assertEquals(value, result)
  }

  @Test def testRadix10Pos(): Unit = {
    val value = "2387627892347567398736473476"
    val radix = 10
    val aNumber = new BigInteger(value, radix)
    val result = aNumber.toString(radix)
    assertEquals(value, result)
  }

  @Test def testRadix1610Neg(): Unit = {
    val value = "-2489756308572364789878394872984"
    val radix = 16
    val aNumber = new BigInteger(value, radix)
    val result = aNumber.toString(radix)
    assertEquals(value, result)
  }

  @Test def testRadix1610Pos(): Unit = {
    val value = "2387627892347567398736473476"
    val radix = 16
    val aNumber = new BigInteger(value, radix)
    val result = aNumber.toString(radix)
    assertEquals(value, result)
  }

  @Test def testRadix16Neg(): Unit = {
    val value = "-287628a883451b800865c67e8d7ff20"
    val radix = 16
    val aNumber = new BigInteger(value, radix)
    val result = aNumber.toString(radix)
    assertEquals(value, result)
  }

  @Test def testRadix16Pos(): Unit = {
    val value = "287628a883451b800865c67e8d7ff20"
    val radix = 16
    val aNumber = new BigInteger(value, radix)
    val result = aNumber.toString(radix)
    assertEquals(value, result)
  }

  @Test def testRadix24Neg(): Unit = {
    val value = "-287628a88gmn3451b8ijk00865c67e8d7ff20"
    val radix = 24
    val aNumber = new BigInteger(value, radix)
    val result = aNumber.toString(radix)
    assertEquals(value, result)
  }

  @Test def testRadix24Pos(): Unit = {
    val value = "287628a883451bg80ijhk0865c67e8d7ff20"
    val radix = 24
    val aNumber = new BigInteger(value, radix)
    val result = aNumber.toString(radix)
    assertEquals(value, result)
  }

  @Test def testRadix2Neg(): Unit = {
    val value = "-101001100010010001001010101110000101010110001010010101010101010101010101010101010101010101010010101"
    val radix = 2
    val aNumber = new BigInteger(value, radix)
    val result = aNumber.toString(radix)
    assertEquals(value, result)
  }

  @Test def testRadix2Pos(): Unit = {
    val value = "101000011111000000110101010101010101010001001010101010101010010101010101010000100010010"
    val radix = 2
    val aNumber = new BigInteger(value, radix)
    val result = aNumber.toString(radix)
    assertEquals(value, result)
  }

  @Test def testRadix36Neg(): Unit = {
    val value = "-uhguweut98iu4h3478tq3985pq98yeiuth33485yq4aiuhalai485yiaehasdkr8tywi5uhslei8"
    val radix = 36
    val aNumber = new BigInteger(value, radix)
    val result = aNumber.toString(radix)
    assertEquals(value, result)
  }

  @Test def testRadix36Pos(): Unit = {
    val value = "23895lt45y6vhgliuwhgi45y845htsuerhsi4586ysuerhtsio5y68peruhgsil4568ypeorihtse48y6"
    val radix = 36
    val aNumber = new BigInteger(value, radix)
    val result = aNumber.toString(radix)
    assertEquals(value, result)
  }

  @Test def testRadixOutOfRange(): Unit = {
    val value = "442429234853876401"
    val radix = 10
    val aNumber = new BigInteger(value, radix)
    val result = aNumber.toString(45)
    assertEquals(value, result)
  }
}
