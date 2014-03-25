/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package javalib

import java.lang.{Short => JShort}

/**
 * tests the implementation of the java standard library Short
 */
object ShortTest extends JasmineTest {

  describe("java.lang.Short") {

    it("should provide `compareTo`") {
      def compare(x: Short, y: Short): Int =
        new JShort(x).compareTo(new JShort(y))

      expect(compare(0.toShort, 5.toShort)).toBeLessThan(0)
      expect(compare(10.toShort, 9.toShort)).toBeGreaterThan(0)
      expect(compare(-2.toShort, -1.toShort)).toBeLessThan(0)
      expect(compare(3.toShort, 3.toShort)).toEqual(0)
    }

    it("should be a Comparable") {
      def compare(x: Any, y: Any): Int =
        x.asInstanceOf[Comparable[Any]].compareTo(y)

      expect(compare(0.toShort, 5.toShort)).toBeLessThan(0)
      expect(compare(10.toShort, 9.toShort)).toBeGreaterThan(0)
      expect(compare(-2.toShort, -1.toShort)).toBeLessThan(0)
      expect(compare(3.toShort, 3.toShort)).toEqual(0)
    }

  }
}
