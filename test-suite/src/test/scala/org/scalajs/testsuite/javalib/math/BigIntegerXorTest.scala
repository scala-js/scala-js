/*
 *  Ported by Alistair Johnson from https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerXorTest.java
 */

package org.scalajs.testsuite.javalib.math

import java.math.BigInteger
import org.scalajs.jasminetest.JasmineTest

object BigIntegerXorTest extends JasmineTest {

  describe("BigIntegerXorTest") {

    it("testNegNegFirstLonger") {
      val numA = "-2837462783428374767845648748973847593874837948575684767"
      val numB = "-293478573489347658763745839457637"
      val res = "2837462783428374767845615168483972194300564226167553530"
      val aNumber = new BigInteger(numA)
      val bNumber = new BigInteger(numB)
      val result = aNumber.xor(bNumber)
      expect(res).toEqual(result.toString)
    }

    it("testNegNegFirstShorter") {
      val numA = "293478573489347658763745839457637"
      val numB = "2837462783428374767845648748973847593874837948575684767"
      val res = "2837462783428374767845615168483972194300564226167553530"
      val aNumber = new BigInteger(numA)
      val bNumber = new BigInteger(numB)
      val result = aNumber.xor(bNumber)
      expect(res).toEqual(result.toString)
    }

    it("testNegNegSameLength") {
      val numA = "-283746278342837476784564875684767"
      val numB = "-293478573489347658763745839457637"
      val res = "71412358434940908477702819237626"
      val aNumber = new BigInteger(numA)
      val bNumber = new BigInteger(numB)
      val result = aNumber.xor(bNumber)
      expect(res).toEqual(result.toString)
    }

    it("testNegPos") {
      val numA = "-27384627835298756289327365"
      val numB = "0"
      val res = "-27384627835298756289327365"
      val aNumber = new BigInteger(numA)
      val bNumber = new BigInteger(numB)
      val result = aNumber.xor(bNumber)
      expect(res).toEqual(result.toString)
    }

    it("testNegPosFirstLonger") {
      val numA = "-2837462783428374767845648748973847593874837948575684767"
      val numB = "293478573489347658763745839457637"
      val res = "-2837462783428374767845615168483972194300564226167553532"
      val aNumber = new BigInteger(numA)
      val bNumber = new BigInteger(numB)
      val result = aNumber.xor(bNumber)
      expect(res).toEqual(result.toString)
    }

    it("testNegPosFirstShorter") {
      val numA = "-293478573489347658763745839457637"
      val numB = "2837462783428374767845648748973847593874837948575684767"
      val res = "-2837462783428374767845615168483972194300564226167553532"
      val aNumber = new BigInteger(numA)
      val bNumber = new BigInteger(numB)
      val result = aNumber.xor(bNumber)
      expect(res).toEqual(result.toString)
    }

    it("testNegPosSameLength") {
      val numA = "-283746278342837476784564875684767"
      val numB = "293478573489347658763745839457637"
      val res = "-71412358434940908477702819237628"
      val aNumber = new BigInteger(numA)
      val bNumber = new BigInteger(numB)
      val result = aNumber.xor(bNumber)
      expect(res).toEqual(result.toString)
    }

    it("testOneOne") {
      val numA = "1"
      val numB = "1"
      val res = "0"
      val aNumber = new BigInteger(numA)
      val bNumber = new BigInteger(numB)
      val result = aNumber.xor(bNumber)
      expect(res).toEqual(result.toString)
    }

    it("testPosNegFirstLonger") {
      val numA = "2837462783428374767845648748973847593874837948575684767"
      val numB = "-293478573489347658763745839457637"
      val res = "-2837462783428374767845615168483972194300564226167553532"
      val aNumber = new BigInteger(numA)
      val bNumber = new BigInteger(numB)
      val result = aNumber.xor(bNumber)
      expect(res).toEqual(result.toString)
    }

    it("testPosNegFirstShorter") {
      val numA = "293478573489347658763745839457637"
      val numB = "-2837462783428374767845648748973847593874837948575684767"
      val res = "-2837462783428374767845615168483972194300564226167553532"
      val aNumber = new BigInteger(numA)
      val bNumber = new BigInteger(numB)
      val result = aNumber.xor(bNumber)
      expect(res).toEqual(result.toString)
    }

    it("testPosNegSameLength") {
      val numA = "283746278342837476784564875684767"
      val numB = "-293478573489347658763745839457637"
      val res = "-71412358434940908477702819237628"
      val aNumber = new BigInteger(numA)
      val bNumber = new BigInteger(numB)
      val result = aNumber.xor(bNumber)
      expect(res).toEqual(result.toString)
    }

    it("testPosPosFirstLonger") {
      val numA = "2837462783428374767845648748973847593874837948575684767"
      val numB = "293478573489347658763745839457637"
      val res = "2837462783428374767845615168483972194300564226167553530"
      val aNumber = new BigInteger(numA)
      val bNumber = new BigInteger(numB)
      val result = aNumber.xor(bNumber)
      expect(res).toEqual(result.toString)
    }

    it("testPosPosFirstShorter") {
      val numA = "293478573489347658763745839457637"
      val numB = "2837462783428374767845648748973847593874837948575684767"
      val res = "2837462783428374767845615168483972194300564226167553530"
      val aNumber = new BigInteger(numA)
      val bNumber = new BigInteger(numB)
      val result = aNumber.xor(bNumber)
      expect(res).toEqual(result.toString)
    }

    it("testPosPosSameLength") {
      val numA = "283746278342837476784564875684767"
      val numB = "293478573489347658763745839457637"
      val res = "71412358434940908477702819237626"
      val aNumber = new BigInteger(numA)
      val bNumber = new BigInteger(numB)
      val result = aNumber.xor(bNumber)
      expect(res).toEqual(result.toString)
    }

    it("testPosZero") {
      val numA = "27384627835298756289327365"
      val numB = "0"
      val res = "27384627835298756289327365"
      val aNumber = new BigInteger(numA)
      val bNumber = new BigInteger(numB)
      val result = aNumber.xor(bNumber)
      expect(res).toEqual(result.toString)
    }

    it("testZeroNeg") {
      val numA = "0"
      val numB = "-27384627835298756289327365"
      val res = "-27384627835298756289327365"
      val aNumber = new BigInteger(numA)
      val bNumber = new BigInteger(numB)
      val result = aNumber.xor(bNumber)
      expect(res).toEqual(result.toString)
    }

    it("testZeroOne") {
      val numA = "0"
      val numB = "1"
      val res = "1"
      val aNumber = new BigInteger(numA)
      val bNumber = new BigInteger(numB)
      val result = aNumber.xor(bNumber)
      expect(res).toEqual(result.toString)
    }

    it("testZeroPos") {
      val numA = "0"
      val numB = "27384627835298756289327365"
      val res = "27384627835298756289327365"
      val aNumber = new BigInteger(numA)
      val bNumber = new BigInteger(numB)
      val result = aNumber.xor(bNumber)
      expect(res).toEqual(result.toString)
    }

    it("testZeroZero") {
      val numA = "0"
      val numB = "0"
      val res = "0"
      val aNumber = new BigInteger(numA)
      val bNumber = new BigInteger(numB)
      val result = aNumber.xor(bNumber)
      expect(res).toEqual(result.toString)
    }
  }
}
