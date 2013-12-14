/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package compiler

import scala.scalajs.js

/**
 * tests the compiler re-patching of native longs to
 * scala.scalajs.runtime.Long
 * see scala.scalajs.test.jsinterop.RuntimeLongTest
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
    
    it("primitives should convert to Long") {
      // Byte
      expect(234.toByte.toLong == 234L).toBeTruthy
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
    
    it("should generate a hash") {
      val x = 5L
      val y = 5L
      val z = 6L
      expect(x.##).toEqual(y.##)
      expect(z.##).not.toEqual(y.##)
    }

    it("should correctly concat to string") {
      val x = 20L
      expect("asdf" + 5L + x + "hello").toEqual("asdf520hello")
      expect(x + "hello").toEqual("20hello")
    }
    
    it("string should convert to Long") {
      expect("45678901234567890".toLong == 45678901234567890L).toBeTruthy
    }
    
    it("should convert from and to js.Number") {
      val x = 5: js.Number
      expect((5L: js.Number) == x).toBeTruthy
      expect(x.toLong == 5L).toBeTruthy
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
      expect(() => dyn.asInstanceOf[Int]).toThrow
            
      expect(dyn.isInstanceOf[Long]).toBeTruthy
      expect(dyn.isInstanceOf[Int]).toBeFalsy
    }
    
    it("should correctly compare to other numeric types") {
      expect(5L == 5).toBeTruthy
      expect(5 == 5L).toBeTruthy
      expect(4 == 5l).toBeFalsy
      expect('A' == 65L).toBeTruthy
    }
  }
  
}