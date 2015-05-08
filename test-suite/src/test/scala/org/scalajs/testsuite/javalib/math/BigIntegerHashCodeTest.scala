/*
 *  Ported by Alistair Johnson from https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/math/BigIntegerHashCodeTest.java
 */
package org.scalajs.testsuite.javalib.math

import java.math.BigInteger
import org.scalajs.jasminetest.JasmineTest

object BigIntegerHashCodeTest extends JasmineTest {

  describe("BigIntegerHashCodeTest") {

    it("testEqualObjects") {
      val value1 = "12378246728727834290276457386374882976782849"
      val value2 = "12378246728727834290276457386374882976782849"
      val aNumber1 = new BigInteger(value1)
      val aNumber2 = new BigInteger(value2)
      val code1 = aNumber1.hashCode
      val code2 = aNumber2.hashCode
      if (aNumber1 == aNumber2) {
        expect(code1).toEqual(code2)
      }
    }

    it("testSameObject") {
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
      expect(code1).toEqual(code2)
    }

    it("testUnequalObjectsUnequal") {
      val value1 = "12378246728727834290276457386374882976782849"
      val value2 = "-5634562095872038262928728727834290276457386374882976782849"
      val aNumber1 = new BigInteger(value1)
      val aNumber2 = new BigInteger(value2)
      val code1 = aNumber1.hashCode
      val code2 = aNumber2.hashCode
      if (aNumber1 != aNumber2) {
        expect(code1).not.toEqual(code2)
      }
    }
  }
}
