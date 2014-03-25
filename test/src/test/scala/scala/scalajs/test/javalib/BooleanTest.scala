/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package javalib

import java.lang.{Boolean => JBoolean}

/**
 * tests the implementation of the java standard library Boolean
 */
object BooleanTest extends JasmineTest {

  describe("java.lang.Boolean") {

    it("should provide `compareTo`") {
      def compare(x: Boolean, y: Boolean): Int =
        new JBoolean(x).compareTo(new JBoolean(y))

      expect(compare(false, false)).toEqual(0)
      expect(compare(false, true)).toBeLessThan(0)
      expect(compare(true, false)).toBeGreaterThan(0)
      expect(compare(true, true)).toEqual(0)
    }

    it("should be a Comparable") {
      def compare(x: Any, y: Any): Int =
        x.asInstanceOf[Comparable[Any]].compareTo(y)

      expect(compare(false, false)).toEqual(0)
      expect(compare(false, true)).toBeLessThan(0)
      expect(compare(true, false)).toBeGreaterThan(0)
      expect(compare(true, true)).toEqual(0)
    }

    it("should parse strings") {
      def test(s: String, v: Boolean): Unit = {
        expect(JBoolean.parseBoolean(s)).toEqual(v)
        expect(JBoolean.valueOf(s).booleanValue()).toEqual(v)
        expect(new JBoolean(s).booleanValue()).toEqual(v)
      }

      test("false", false)
      test("true", true)
      test("TrUe", true)
      test(null, false)
      test("truee", false)
    }

  }
}
