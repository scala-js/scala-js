/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.math

import java.math.BigDecimal

import org.scalajs.jasminetest.JasmineTest

object BigDecimalTest extends JasmineTest  {

  describe("java.lang.Math.BigDecimal Int/Long Constructors") {

    it("should accept 3 as aLong") {
      val bd = BigDecimal.valueOf(3L)
      expect(bd.intValue()).toEqual(3)
      expect(bd.longValue == 3L).toBeTruthy()
    }
    
    it("should accept 999999999 as aLong") {
      val bd = BigDecimal.valueOf(999999999L)
      expect(bd.intValue()).toEqual(999999999)
      expect(bd.longValue == 999999999L).toBeTruthy()
    }

    it("should accept 9999999999 as aLong") {
      val bd = BigDecimal.valueOf(9999999999L)
      expect(bd.longValue == 9999999999L).toBeTruthy()
    }

    it("should accept -999999999 as aLong") {
      val bd = BigDecimal.valueOf(-999999999L)
      expect(bd.intValue()).toEqual(-999999999)
      expect(bd.longValue == -999999999L).toBeTruthy()
    }

    it("should accept -9999999999 as aLong") {
      val bd = BigDecimal.valueOf(-9999999999L)
      expect(bd.longValue == -9999999999L).toBeTruthy()
    }

    it("should accept 3 as a string") {
      val bd = new BigDecimal("3")
      expect(bd.intValue()).toEqual(3)
      expect(bd.longValue == 3L).toBeTruthy()
    }

    it("should accept 99 as a string") {
      val bd = new BigDecimal("99")
      expect(bd.intValue()).toEqual(99)
      expect(bd.longValue == 99L).toBeTruthy()
    }

    it("should accept 999999999 as string") {
      val bd = new BigDecimal("999999999")
      expect(bd.intValue()).toEqual(999999999)
      expect(bd.longValue == 999999999L).toBeTruthy()
    }

    it("should accept 9999999999 as a string") {
      val bd = new BigDecimal("9999999999")
      expect(bd.longValue == 9999999999L).toBeTruthy()
    }

    it("should accept -99 as a string") {
      val bd = new BigDecimal("-99")
      expect(bd.intValue()).toEqual(-99)
      expect(bd.longValue == -99L).toBeTruthy()
    }

    it("should accept -999999999 as sting") {
      val bd = new BigDecimal("-999999999")
      expect(bd.intValue()).toEqual(-999999999)
      expect(bd.longValue == -999999999L).toBeTruthy()
    }

    it("should accept -9999999999 as a string") {
      val bd = new BigDecimal("-9999999999")
      expect(bd.longValue == -9999999999L).toBeTruthy()
    }

    it("should accept 9.9 as a string") {
      val bd = new BigDecimal("9.9")
      expect(bd.toString).toEqual("9.9")
      expect(bd.doubleValue()).toEqual(9.9)
    }

    it("should accept 99.99 as a string") {
      val bd = new BigDecimal("99.99")
      expect(bd.doubleValue()).toEqual(99.99)
    }

    it("should accept 999.999 as a string") {
      val bd = new BigDecimal("999.999")
      expect(bd.doubleValue()).toEqual(999.999)
    }

    it("should accept 9999.9999 as a string") {
      val bd = new BigDecimal("9999.9999")
      expect(bd.doubleValue()).toEqual(9999.9999)
    }
  }

  describe("java.lang.Math.BigDecimal double Constructors") {

    it("should accept 3.3 as a double") {
      val d = 3.3
      val bd = new BigDecimal(d)
      expect(bd.doubleValue()).toEqual(d)
    }

    it("should accept 99.99 as a double") {
      val d = 99.99
      val bd = new BigDecimal(d)
      expect(bd.doubleValue()).toEqual(d)
    }

    it("should accept 9999.9999 as a double") {
      val d:Double = 9999.9999
      val bd = new BigDecimal(d)
      expect(bd.doubleValue()).toEqual(d)
    }

    it("should accept 99999999.99999999 as a double") {
      val d = 99999999.99999999
      val bd = new BigDecimal(d)
      expect(bd.doubleValue()).toEqual(d)
    }

    it("should accept 999999999.999999999 as a double") {
      val d = 999999999.999999999
      val bd = new BigDecimal(d)
      expect(bd.doubleValue()).toEqual(d)
    }

    it("should accept 9999999999.9999999999 as a double") {
      val d = 9999999999.9999999999
      val bd = new BigDecimal(d)
      expect(bd.doubleValue()).toEqual(d)
    }

    it("should accept -3.3 as a double") {
      val d = -3.3
      val bd = new BigDecimal(d)
      expect(bd.doubleValue()).toEqual(d)
    }

    it("should accept -99.99 as a double") {
      val d = -99.99
      val bd = new BigDecimal(d)
      expect(bd.doubleValue()).toEqual(d)
    }

    it("should accept -99999999.99999999 as a double") {
      val d = -99999999.99999999
      val bd = new BigDecimal(d)
      expect(bd.doubleValue()).toEqual(d)
    }

    it("should accept -999999999.999999999 as a double") {
      val d = -999999999.999999999
      val bd = new BigDecimal(d)
      expect(bd.doubleValue()).toEqual(d)
    }

    it("should accept -9999999999.9999999999 as a double") {
      val d = -9999999999.9999999999
      val bd = new BigDecimal(d)
      expect(bd.doubleValue()).toEqual(d)
    }
  }
}
