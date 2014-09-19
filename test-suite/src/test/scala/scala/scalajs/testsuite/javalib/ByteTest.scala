/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.testsuite.javalib

import java.lang.{Byte => JByte}

import org.scalajs.jasminetest.JasmineTest

/**
 * tests the implementation of the java standard library Byte
 */
object ByteTest extends JasmineTest {

  describe("java.lang.Byte") {

    it("should provide `compareTo`") {
      def compare(x: Byte, y: Byte): Int =
        new JByte(x).compareTo(new JByte(y))

      expect(compare(0.toByte, 5.toByte)).toBeLessThan(0)
      expect(compare(10.toByte, 9.toByte)).toBeGreaterThan(0)
      expect(compare(-2.toByte, -1.toByte)).toBeLessThan(0)
      expect(compare(3.toByte, 3.toByte)).toEqual(0)
    }

    it("should be a Comparable") {
      def compare(x: Any, y: Any): Int =
        x.asInstanceOf[Comparable[Any]].compareTo(y)

      expect(compare(0.toByte, 5.toByte)).toBeLessThan(0)
      expect(compare(10.toByte, 9.toByte)).toBeGreaterThan(0)
      expect(compare(-2.toByte, -1.toByte)).toBeLessThan(0)
      expect(compare(3.toByte, 3.toByte)).toEqual(0)
    }

    it("should parse strings") {
      def test(s: String, v: Byte): Unit = {
        expect(JByte.parseByte(s)).toEqual(v)
        expect(JByte.valueOf(s).byteValue()).toEqual(v)
        expect(new JByte(s).byteValue()).toEqual(v)
      }

      test("0", 0)
      test("5", 5)
      test("127", 127)
      test("-100", -100)
    }

    it("should reject invalid strings when parsing") {
      def test(s: String): Unit =
        expect(() => JByte.parseByte(s)).toThrow

      test("abc")
      test("")
      test("200") // out of range
    }

  }
}
