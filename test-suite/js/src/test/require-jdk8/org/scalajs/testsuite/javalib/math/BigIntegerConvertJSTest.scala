package org.scalajs.testsuite.javalib.math

import java.math.BigInteger

import org.scalajs.jasminetest.JasmineTest

object BigIntegerConvertJSTest extends JasmineTest {
  describe("BigIntegerConvertTest") {
    // To test that it works with strict floats, do:
    //   > set scalaJSSemantics in testSuite ~= { _.withStrictFloats(true) }
    when("strict-floats").
    it("testDoubleValueZero") {
      val a = "0"
      val result = 0.0
      val aNumber = new BigInteger(a).doubleValue()
      expect(aNumber).toEqual(result)
    }
  }
}
