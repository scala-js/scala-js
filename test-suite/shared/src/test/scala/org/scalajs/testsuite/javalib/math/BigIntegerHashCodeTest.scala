// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerHashCodeTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math.BigInteger

import org.junit.Test
import org.junit.Assert._

class BigIntegerHashCodeTest {

  @Test def testEqualObjects(): Unit = {
    val value1 = "12378246728727834290276457386374882976782849"
    val value2 = "12378246728727834290276457386374882976782849"
    val aNumber1 = new BigInteger(value1)
    val aNumber2 = new BigInteger(value2)
    val code1 = aNumber1.hashCode
    val code2 = aNumber2.hashCode
    if (aNumber1 == aNumber2) {
      assertEquals(code2, code1)
    }
  }

  @Test def hashCodeIssue2159(): Unit = {
    val a = 936417286865811553L
    val b = 1136802186495971562L
    val c = BigInteger.valueOf(a).add(BigInteger.valueOf(b))
    val d = BigInteger.valueOf(c.longValue())
    assertEquals(c, d) // sanity
    assertEquals(c.hashCode, d.hashCode)
  }

  @Test def testSameObject(): Unit = {
    val value1 = "12378246728727834290276457386374882976782849"
    val value2 = "-5634562095872038262928728727834290276457386374882976782849"
    val aNumber1 = new BigInteger(value1)
    val aNumber2 = new BigInteger(value2)
    val code1 = aNumber1.hashCode
    aNumber1.add(aNumber2).shiftLeft(125)
    aNumber1.subtract(aNumber2).shiftRight(125)
    aNumber1.multiply(aNumber2).toByteArray()
    aNumber1.divide(aNumber2).bitLength()
    aNumber1.gcd(aNumber2).pow(7)
    val code2 = aNumber1.hashCode
    assertEquals(code2, code1)
  }

  @Test def testUnequalObjectsUnequal(): Unit = {
    val value1 = "12378246728727834290276457386374882976782849"
    val value2 = "-5634562095872038262928728727834290276457386374882976782849"
    val aNumber1 = new BigInteger(value1)
    val aNumber2 = new BigInteger(value2)
    val code1 = aNumber1.hashCode
    val code2 = aNumber2.hashCode
    if (aNumber1 != aNumber2) {
      assertNotEquals(code1, code2)
    }
  }
}
