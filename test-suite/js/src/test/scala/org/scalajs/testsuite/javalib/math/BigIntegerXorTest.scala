// scalastyle:off line.size.limit
/*
 * Ported by Alistair Johnson from
 * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerXorTest.java
 */
// scalastyle:on line.size.limit

package org.scalajs.testsuite.javalib.math

import java.math.BigInteger

import org.junit.Test
import org.junit.Assert._

class BigIntegerXorTest {

  @Test def testNegNegFirstLonger(): Unit = {
    val numA = "-2837462783428374767845648748973847593874837948575684767"
    val numB = "-293478573489347658763745839457637"
    val res = "2837462783428374767845615168483972194300564226167553530"
    val aNumber = new BigInteger(numA)
    val bNumber = new BigInteger(numB)
    val result = aNumber.xor(bNumber)
    assertEquals(result.toString, res)
  }

  @Test def testNegNegFirstShorter(): Unit = {
    val numA = "293478573489347658763745839457637"
    val numB = "2837462783428374767845648748973847593874837948575684767"
    val res = "2837462783428374767845615168483972194300564226167553530"
    val aNumber = new BigInteger(numA)
    val bNumber = new BigInteger(numB)
    val result = aNumber.xor(bNumber)
    assertEquals(result.toString, res)
  }

  @Test def testNegNegSameLength(): Unit = {
    val numA = "-283746278342837476784564875684767"
    val numB = "-293478573489347658763745839457637"
    val res = "71412358434940908477702819237626"
    val aNumber = new BigInteger(numA)
    val bNumber = new BigInteger(numB)
    val result = aNumber.xor(bNumber)
    assertEquals(result.toString, res)
  }

  @Test def testNegPos(): Unit = {
    val numA = "-27384627835298756289327365"
    val numB = "0"
    val res = "-27384627835298756289327365"
    val aNumber = new BigInteger(numA)
    val bNumber = new BigInteger(numB)
    val result = aNumber.xor(bNumber)
    assertEquals(result.toString, res)
  }

  @Test def testNegPosFirstLonger(): Unit = {
    val numA = "-2837462783428374767845648748973847593874837948575684767"
    val numB = "293478573489347658763745839457637"
    val res = "-2837462783428374767845615168483972194300564226167553532"
    val aNumber = new BigInteger(numA)
    val bNumber = new BigInteger(numB)
    val result = aNumber.xor(bNumber)
    assertEquals(result.toString, res)
  }

  @Test def testNegPosFirstShorter(): Unit = {
    val numA = "-293478573489347658763745839457637"
    val numB = "2837462783428374767845648748973847593874837948575684767"
    val res = "-2837462783428374767845615168483972194300564226167553532"
    val aNumber = new BigInteger(numA)
    val bNumber = new BigInteger(numB)
    val result = aNumber.xor(bNumber)
    assertEquals(result.toString, res)
  }

  @Test def testNegPosSameLength(): Unit = {
    val numA = "-283746278342837476784564875684767"
    val numB = "293478573489347658763745839457637"
    val res = "-71412358434940908477702819237628"
    val aNumber = new BigInteger(numA)
    val bNumber = new BigInteger(numB)
    val result = aNumber.xor(bNumber)
    assertEquals(result.toString, res)
  }

  @Test def testOneOne(): Unit = {
    val numA = "1"
    val numB = "1"
    val res = "0"
    val aNumber = new BigInteger(numA)
    val bNumber = new BigInteger(numB)
    val result = aNumber.xor(bNumber)
    assertEquals(result.toString, res)
  }

  @Test def testPosNegFirstLonger(): Unit = {
    val numA = "2837462783428374767845648748973847593874837948575684767"
    val numB = "-293478573489347658763745839457637"
    val res = "-2837462783428374767845615168483972194300564226167553532"
    val aNumber = new BigInteger(numA)
    val bNumber = new BigInteger(numB)
    val result = aNumber.xor(bNumber)
    assertEquals(result.toString, res)
  }

  @Test def testPosNegFirstShorter(): Unit = {
    val numA = "293478573489347658763745839457637"
    val numB = "-2837462783428374767845648748973847593874837948575684767"
    val res = "-2837462783428374767845615168483972194300564226167553532"
    val aNumber = new BigInteger(numA)
    val bNumber = new BigInteger(numB)
    val result = aNumber.xor(bNumber)
    assertEquals(result.toString, res)
  }

  @Test def testPosNegSameLength(): Unit = {
    val numA = "283746278342837476784564875684767"
    val numB = "-293478573489347658763745839457637"
    val res = "-71412358434940908477702819237628"
    val aNumber = new BigInteger(numA)
    val bNumber = new BigInteger(numB)
    val result = aNumber.xor(bNumber)
    assertEquals(result.toString, res)
  }

  @Test def testPosPosFirstLonger(): Unit = {
    val numA = "2837462783428374767845648748973847593874837948575684767"
    val numB = "293478573489347658763745839457637"
    val res = "2837462783428374767845615168483972194300564226167553530"
    val aNumber = new BigInteger(numA)
    val bNumber = new BigInteger(numB)
    val result = aNumber.xor(bNumber)
    assertEquals(result.toString, res)
  }

  @Test def testPosPosFirstShorter(): Unit = {
    val numA = "293478573489347658763745839457637"
    val numB = "2837462783428374767845648748973847593874837948575684767"
    val res = "2837462783428374767845615168483972194300564226167553530"
    val aNumber = new BigInteger(numA)
    val bNumber = new BigInteger(numB)
    val result = aNumber.xor(bNumber)
    assertEquals(result.toString, res)
  }

  @Test def testPosPosSameLength(): Unit = {
    val numA = "283746278342837476784564875684767"
    val numB = "293478573489347658763745839457637"
    val res = "71412358434940908477702819237626"
    val aNumber = new BigInteger(numA)
    val bNumber = new BigInteger(numB)
    val result = aNumber.xor(bNumber)
    assertEquals(result.toString, res)
  }

  @Test def testPosZero(): Unit = {
    val numA = "27384627835298756289327365"
    val numB = "0"
    val res = "27384627835298756289327365"
    val aNumber = new BigInteger(numA)
    val bNumber = new BigInteger(numB)
    val result = aNumber.xor(bNumber)
    assertEquals(result.toString, res)
  }

  @Test def testZeroNeg(): Unit = {
    val numA = "0"
    val numB = "-27384627835298756289327365"
    val res = "-27384627835298756289327365"
    val aNumber = new BigInteger(numA)
    val bNumber = new BigInteger(numB)
    val result = aNumber.xor(bNumber)
    assertEquals(result.toString, res)
  }

  @Test def testZeroOne(): Unit = {
    val numA = "0"
    val numB = "1"
    val res = "1"
    val aNumber = new BigInteger(numA)
    val bNumber = new BigInteger(numB)
    val result = aNumber.xor(bNumber)
    assertEquals(result.toString, res)
  }

  @Test def testZeroPos(): Unit = {
    val numA = "0"
    val numB = "27384627835298756289327365"
    val res = "27384627835298756289327365"
    val aNumber = new BigInteger(numA)
    val bNumber = new BigInteger(numB)
    val result = aNumber.xor(bNumber)
    assertEquals(result.toString, res)
  }

  @Test def testZeroZero(): Unit = {
    val numA = "0"
    val numB = "0"
    val res = "0"
    val aNumber = new BigInteger(numA)
    val bNumber = new BigInteger(numB)
    val result = aNumber.xor(bNumber)
    assertEquals(result.toString, res)
  }
}
