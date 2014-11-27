/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import java.util.Date

import org.scalajs.jasminetest.JasmineTest

/**
 * tests the implementation of the java standard library Date
 */
object DateTest extends JasmineTest {

  describe("java.lang.Date") {

    it("should provide `compareTo`") {
      def compare(x: Date, y: Date): Int = {
        x.compareTo(y)
      }

      expect(compare(new Date(97, 11, 5, 0, 0), new Date(98, 11, 5, 0, 0))).toBeLessThan(0)
      expect(compare(new Date(98, 11, 5, 0, 0), new Date(97, 11, 5, 0, 0))).toBeGreaterThan(0)
      expect(compare(new Date(97, 11, 5, 0, 0), new Date(97, 11, 5))).toEqual(0)
      expect(compare(new Date(97, 11, 5, 0, 0), new Date(97, 11, 5, 0, 1))).toBeLessThan(0)
      expect(compare(new Date(97, 11, 5), new Date(97, 11, 5, 0, 0))).toEqual(0)
    }

    it("should be a Comparable") {
      def compare(x: Any, y: Any): Int =
        x.asInstanceOf[Comparable[Any]].compareTo(y)

      expect(compare(new Date(97, 11, 5, 0, 0), new Date(98, 11, 5, 0, 0))).toBeLessThan(0)
      expect(compare(new Date(98, 11, 5, 0, 0), new Date(97, 11, 5, 0, 0))).toBeGreaterThan(0)
      expect(compare(new Date(97, 11, 5, 0, 0), new Date(97, 11, 5))).toEqual(0)
      expect(compare(new Date(97, 11, 5, 0, 0), new Date(97, 11, 5, 0, 1))).toBeLessThan(0)
      expect(compare(new Date(97, 11, 5), new Date(97, 11, 5, 0, 0))).toEqual(0)
    }

    it("should parse strings") {
      def test(s: String, v: Date): Unit =
        expect(new Date(s).compareTo(v)).toEqual(0)

      test("Nov 5 1997 5:23:27 GMT", new Date(Date.UTC(97, 10, 5, 5, 23, 27)))
      test("Nov 1 1997 GMT", new Date(Date.UTC(97,10,1, 0, 0, 0)))
      test("Jan 1 1970 18:11:01 GMT", new Date(Date.UTC(70,0,1,18,11,1)))
    }

    it("should provide after") {
      expect(new Date(97, 11, 5, 0, 0).after(new Date(98, 11, 5, 0, 0))).toBe(false)
      expect(new Date(99, 11, 5, 0, 0).after(new Date(98, 11, 5, 0, 0))).toBe(true)
      expect(new Date(99, 11, 5, 0, 0).after(new Date(99, 11, 5, 0, 0))).toBe(false)
    }

    it("should provide before") {
      expect(new Date(97, 11, 5, 0, 0).before(new Date(98, 11, 5, 0, 0))).toBe(true)
      expect(new Date(99, 11, 5, 0, 0).before(new Date(98, 11, 5, 0, 0))).toBe(false)
      expect(new Date(99, 11, 5, 0, 0).before(new Date(99, 11, 5, 0, 0))).toBe(false)
    }

    it("should provide clone") {
      def testClone(date: Date) = {
        val cloned = date.clone()
        date == cloned
      }

      expect(testClone(new Date(97, 11, 5, 0, 0))).toBe(true)
      expect(testClone(new Date(92, 14, 6, 2, 1))).toBe(true)
      expect(testClone(new Date(4, 1, 2, 3, 0, 0))).toBe(true)
    }

    it("should respond to getYear") {
      def testYear(year: Int) = {
        val date = new Date()
        date.setYear(year)
        expect(date.getYear).toEqual(year)
      }
      testYear(1940)
      testYear(1920)
      testYear(2030)
    }
  }
}
