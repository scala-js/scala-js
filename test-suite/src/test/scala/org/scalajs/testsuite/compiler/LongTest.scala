/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import scala.scalajs.js

import org.scalajs.jasminetest.JasmineTest

/**
 * tests the compiler re-patching of native longs to
 * scala.scalajs.runtime.Long
 * see org.scalajs.testsuite.jsinterop.RuntimeLongTest
 * for a test of the implementation itself
 */
object LongTest extends JasmineTest {

  describe("JavaScript 64-bit long compatibility") {
    it("should correctly handle literals") {
      expect(5L + 100L == 105L).toBeTruthy
      expect(2147483649L + 2L == 2147483651L).toBeTruthy
      expect(-2147483648L * 4 == -8589934592L).toBeTruthy
      expect(4503599627370510L * (-4) == -18014398509482040L).toBeTruthy
    }

    it("should correctly dispatch unary ops on Longs") {
      val x = 10L
      expect(-x == -10L).toBeTruthy
      val y = 5L
      expect(-y == -5L).toBeTruthy
      expect(+y == 5L).toBeTruthy
      expect(~y == -6L).toBeTruthy
    }

    it("should correctly dispatch binary ops on Longs") {
      expect(5L * 5F == 25F).toBeTruthy
      expect(5L % 4F == 1F).toBeTruthy
      expect(5F * 4L == 20F).toBeTruthy
    }

    it("should support shifts with Longs - #622") {
      def l(x: Long): Long = x
      def i(x: Int): Int = x

      expect(l(-7L) >>> 100L == 268435455L).toBeTruthy
      expect(l(-7L) >> 100L == -1L).toBeTruthy
      expect(l(-7L) >> 100 == -1L).toBeTruthy
      expect(l(-7L) >>> 100 == 268435455).toBeTruthy
      expect(l(-7L) << 100L == -481036337152L).toBeTruthy
      expect(l(-7L) << 100 == -481036337152L).toBeTruthy
      expect(l(7L) << 100L == 481036337152L).toBeTruthy
      expect(l(8L) << 100L == 549755813888L).toBeTruthy
      expect(l(-7L) >>> 4 == 1152921504606846975L).toBeTruthy

      expect(i(7) << 100).toEqual(112)
      expect(i(-7) >> 100).toEqual(-1)
      expect(i(-7) >>> 100).toEqual(268435455)
      expect(i(-65) >> 100).toEqual(-5)
      expect(i(-65) >> 4).toEqual(-5)
    }

    it("primitives should convert to Long") {
      // Byte
      expect(112.toByte.toLong == 112L).toBeTruthy
      // Short
      expect((-10).toShort.toLong == -10L).toBeTruthy
      // Char
      expect('A'.toLong == 65L).toBeTruthy
      // Int
      expect(5.toLong == 5L).toBeTruthy
      // Long
      expect(10L.toLong == 10L).toBeTruthy
      // Float
      expect(100000.6f.toLong == 100000L).toBeTruthy
      // Double
      expect(100000.6.toLong == 100000L).toBeTruthy
    }

    it("should support hashCode()") {
      expect(0L         .hashCode()).toEqual(0)
      expect(55L        .hashCode()).toEqual(55)
      expect((-12L)     .hashCode()).toEqual(11)
      expect(10006548L  .hashCode()).toEqual(10006548)
      expect((-1098748L).hashCode()).toEqual(1098747)

      expect(613354684553L       .hashCode()).toEqual(-825638905)
      expect(9863155567412L      .hashCode()).toEqual(1910653900)
      expect(3632147899696541255L.hashCode()).toEqual(1735398658)
      expect(7632147899696541255L.hashCode()).toEqual(-1689438124)
    }

    it("should support ##") {
      expect(0L         .##).toEqual(0)
      expect(55L        .##).toEqual(55)
      expect((-12L)     .##).toEqual(-12)
      expect(10006548L  .##).toEqual(10006548)
      expect((-1098748L).##).toEqual(-1098748)

      expect(9863155567412L      .##).toEqual(1910653900)
      expect(3632147899696541255L.##).toEqual(1735398658)

      // These two (correctly) give different results on 2.10 and 2.11
      //expect(613354684553L       .##).toEqual(-825638905)  // xx06 on 2.10
      //expect(7632147899696541255L.##).toEqual(-1689438124) // xx25 on 2.10
    }

    it("should correctly concat to string") {
      val x = 20L
      expect("asdf" + 5L + x + "hello").toEqual("asdf520hello")
      expect(x + "hello").toEqual("20hello")
    }

    it("string should convert to Long") {
      expect("45678901234567890".toLong == 45678901234567890L).toBeTruthy
    }

    it("should convert to js.Any") {
      val x = 5: js.Any
      expect((5L: js.Any) == x).toBeTruthy
    }

    it("should correctly implement is/asInstanceOf Longs") {
      val dyn:  Any  = 5L
      val stat: Long = 5L

      expect(stat.asInstanceOf[Long]).toEqual(5L)
      // models current scala behavior. See SI-1448
      expect(stat.asInstanceOf[Int]).toEqual(5)

      expect(stat.isInstanceOf[Long]).toBeTruthy
      expect(stat.isInstanceOf[Int]).toBeFalsy

      expect(dyn.asInstanceOf[Long]).toEqual(5L)

      expect(dyn.isInstanceOf[Long]).toBeTruthy
      expect(dyn.isInstanceOf[Int]).toBeFalsy
    }

    when("compliant-asinstanceof").
    it("should correctly implement asInstanceOf Longs (negative)") {
      val dyn: Any = 5L

      expect(() => dyn.asInstanceOf[Int]).toThrow
    }

    it("should correctly compare to other numeric types") {
      expect(5L == 5).toBeTruthy
      expect(5 == 5L).toBeTruthy
      expect(4 == 5l).toBeFalsy
      expect('A' == 65L).toBeTruthy
    }
  }

}
