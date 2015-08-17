/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang

import org.scalajs.jasminetest.JasmineTest
import scala.scalajs.js

object IntegerTest extends JasmineTest {

  describe("java.lang.Integer") {

    // Explicitly define these as `var`'s to avoid any compile-time constant folding
    val MaxValue: Int = Int.MaxValue
    val MinValue: Int = Int.MinValue

    it("should provide `reverseBytes` used by scala.Enumeration") {
      expect(Integer.reverseBytes(0xdeadbeef)).toEqual(0xefbeadde)
    }

    it("should provide `rotateLeft`") {
      expect(Integer.rotateLeft(0x689cd401, 0)).toEqual(0x689cd401)
      expect(Integer.rotateLeft(0x689cd401, 1)).toEqual(0xd139a802)
      expect(Integer.rotateLeft(0x689cd401, 8)).toEqual(0x9cd40168)
      expect(Integer.rotateLeft(0x689cd401, 13)).toEqual(0x9a802d13)
      expect(Integer.rotateLeft(0x689cd401, 32)).toEqual(0x689cd401)
      expect(Integer.rotateLeft(0x689cd401, 33)).toEqual(0xd139a802)
      expect(Integer.rotateLeft(0x689cd401, 43)).toEqual(0xe6a00b44)
      expect(Integer.rotateLeft(0x689cd401, -1)).toEqual(0xb44e6a00)
      expect(Integer.rotateLeft(0x689cd401, -28)).toEqual(0x89cd4016)
      expect(Integer.rotateLeft(0x689cd401, -39)).toEqual(0x2d139a8)
    }

    it("should provide `rotateRight`") {
      expect(Integer.rotateRight(0x689cd401, 0)).toEqual(0x689cd401)
      expect(Integer.rotateRight(0x689cd401, 1)).toEqual(0xb44e6a00)
      expect(Integer.rotateRight(0x689cd401, 8)).toEqual(0x1689cd4)
      expect(Integer.rotateRight(0x689cd401, 13)).toEqual(0xa00b44e6)
      expect(Integer.rotateRight(0x689cd401, 32)).toEqual(0x689cd401)
      expect(Integer.rotateRight(0x689cd401, 33)).toEqual(0xb44e6a00)
      expect(Integer.rotateRight(0x689cd401, 43)).toEqual(0x802d139a)
      expect(Integer.rotateRight(0x689cd401, -1)).toEqual(0xd139a802)
      expect(Integer.rotateRight(0x689cd401, -28)).toEqual(0x1689cd40)
      expect(Integer.rotateRight(0x689cd401, -39)).toEqual(0x4e6a00b4)
    }

    it("should provide `bitCount` used by Map") {
      abstract sealed class Status
      case object Used extends Status
      case object Current extends Status
      case object OneMove extends Status
      case object MultipleMoves extends Status
      case object Other extends Status

      val map = Map(Used -> 0, Other -> 0, Current -> 0, MultipleMoves -> 1, OneMove -> 2)

      expect(map.size).toEqual(5)
      expect(map(MultipleMoves)).toEqual(1)
    }

    it("should provide `numberOfTrailingZeros`") {
      expect(Integer.numberOfTrailingZeros(0xa3c49000)).toEqual(12)
      expect(Integer.numberOfTrailingZeros(0x43f49020)).toEqual(5)
      expect(Integer.numberOfTrailingZeros(0x43c08000)).toEqual(15)
      expect(Integer.numberOfTrailingZeros(0)).toEqual(32)
    }

    it("should provide `toBinaryString` for values in range") {
      expect(Integer.toBinaryString(-1)).toEqual("11111111111111111111111111111111")
      expect(Integer.toBinaryString(-10001)).toEqual("11111111111111111101100011101111")
      expect(Integer.toBinaryString(MinValue)).toEqual("10000000000000000000000000000000")
      expect(Integer.toBinaryString(MaxValue)).toEqual("1111111111111111111111111111111")
    }

    it("should provide `toHexString` for values in range") {
      expect(Integer.toHexString(-1)).toEqual("ffffffff")
      expect(Integer.toHexString(-10001)).toEqual("ffffd8ef")
      expect(Integer.toHexString(MinValue)).toEqual("80000000")
      expect(Integer.toHexString(-2147000002)).toEqual("8007613e")
      expect(Integer.toHexString(MaxValue)).toEqual("7fffffff")
    }

    it("should provide `toOctalString` for values in range") {
      expect(Integer.toOctalString(-1)).toEqual("37777777777")
      expect(Integer.toOctalString(-10001)).toEqual("37777754357")
      expect(Integer.toOctalString(MinValue)).toEqual("20000000000")
      expect(Integer.toOctalString(MaxValue)).toEqual("17777777777")
    }

    it("should provide `compareTo`") {
      def compare(x: Int, y: Int): Int =
        new Integer(x).compareTo(new Integer(y))

      expect(compare(0, 5)).toBeLessThan(0)
      expect(compare(10, 9)).toBeGreaterThan(0)
      expect(compare(-2, -1)).toBeLessThan(0)
      expect(compare(3, 3)).toEqual(0)
    }

    it("should be a Comparable") {
      def compare(x: Any, y: Any): Int =
        x.asInstanceOf[Comparable[Any]].compareTo(y)

      expect(compare(0, 5)).toBeLessThan(0)
      expect(compare(10, 9)).toBeGreaterThan(0)
      expect(compare(-2, -1)).toBeLessThan(0)
      expect(compare(3, 3)).toEqual(0)
    }

    it("should parse strings") {
      def test(s: String, v: Int, radix: Int = 10): Unit = {
        expect(Integer.parseInt(s, radix)).toEqual(v)
        expect(Integer.valueOf(s, radix).intValue()).toEqual(v)
        if (radix == 10)
          expect(new Integer(s).intValue()).toEqual(v)
      }

      test("0", 0)
      test("5", 5)
      test("127", 127)
      test("-100", -100)
      test("30000", 30000)
      test("-90000", -90000)
      test("Kona", 411787, 27)
      test("+42", 42)
      test("-0", 0)
      test("-FF", -255, 16)
    }

    it("should reject invalid strings when parsing") {
      def test(s: String, radix: Int = 10): Unit =
        expect(() => Integer.parseInt(s, radix)).toThrow

      test("abc")
      test("5a")
      test("2147483648")
      test("99", 8)
      test("-")
      test("")
    }

    it("should parse strings in base 16") {
      def test(s: String, v: Int): Unit = {
        expect(Integer.parseInt(s, 16)).toEqual(v)
        expect(Integer.valueOf(s, 16).intValue()).toEqual(v)
      }

      test("0", 0x0)
      test("5", 0x5)
      test("ff", 0xff)
      test("-24", -0x24)
      test("30000", 0x30000)
      test("-90000", -0x90000)
    }

    it("should provide `highestOneBit`") {
      /* Spec ported from
       * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/lang/IntegerTest.java
       */
      expect(Integer.highestOneBit(0)).toEqual(0)
      expect(Integer.highestOneBit(-1)).toEqual(Integer.MIN_VALUE)
      expect(Integer.highestOneBit(-256)).toEqual(Integer.MIN_VALUE)
      expect(Integer.highestOneBit(1)).toEqual(1)
      expect(Integer.highestOneBit(0x88)).toEqual(0x80)
      expect(Integer.highestOneBit(Int.MaxValue)).toEqual(0x40000000)
      expect(Integer.highestOneBit(Int.MinValue)).toEqual(Int.MinValue)
    }

    it("should provide `lowestOneBit`") {
      expect(Integer.lowestOneBit(0)).toEqual(0)
      expect(Integer.lowestOneBit(-1)).toEqual(1)
      expect(Integer.lowestOneBit(-256)).toEqual(256)
      expect(Integer.lowestOneBit(12)).toEqual(4)
      expect(Integer.lowestOneBit(0x88)).toEqual(0x8)
      expect(Integer.lowestOneBit(Int.MaxValue)).toEqual(1)
      expect(Integer.lowestOneBit(Int.MinValue)).toEqual(Int.MinValue)
    }

    it("should provide `toString` without radix") {
      /* Spec ported from
       * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/lang/IntegerTest.java
       */
      expect(new Integer(12345).toString).toEqual("12345")
      expect(new Integer("-12345").toString).toEqual("-12345")
      expect(Integer.toString(-80765)).toEqual("-80765")
      expect(Integer.toString(Integer.MAX_VALUE)).toEqual("2147483647")
      expect(Integer.toString(-Integer.MAX_VALUE)).toEqual("-2147483647")
      expect(Integer.toString(Integer.MIN_VALUE)).toEqual("-2147483648")
      expect(Integer.toString(0)).toEqual("0")
    }

    it("should provide `toString` with radix") {
      /* Spec ported from
       * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/lang/IntegerTest.java
       */
      expect(Integer.toString(2147483647, 8)).toEqual("17777777777")
      expect(Integer.toString(2147483647, 16)).toEqual("7fffffff")
      expect(Integer.toString(2147483647, 2)).toEqual("1111111111111111111111111111111")
      expect(Integer.toString(2147483647, 10)).toEqual("2147483647")
      expect(Integer.toString(-2147483647, 8)).toEqual("-17777777777")
      expect(Integer.toString(-2147483647, 16)).toEqual("-7fffffff")
      expect(Integer.toString(-2147483647, 2)).toEqual("-1111111111111111111111111111111")
      expect(Integer.toString(-2147483647, 10)).toEqual("-2147483647")
      expect(Integer.toString(-2147483648, 8)).toEqual("-20000000000")
      expect(Integer.toString(-2147483648, 16)).toEqual("-80000000")
      expect(Integer.toString(-2147483648, 2)).toEqual("-10000000000000000000000000000000")
      expect(Integer.toString(-2147483648, 10)).toEqual("-2147483648")
    }
  }
}
