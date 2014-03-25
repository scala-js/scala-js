/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package javalib

import scala.scalajs.test.JasmineTest
import scala.scalajs.js

object CharacterTest extends JasmineTest {

  describe("java.lang.Character") {

    it("should provide `digit`") {
      expect(Character.digit('a', 16)).toEqual(10)
      expect(Character.digit('}',  5)).toEqual(-1)
      expect(Character.digit('1', 50)).toEqual(-1)
      expect(Character.digit('1', 36)).toEqual(1)
      expect(Character.digit('Z', 36)).toEqual(35)
      expect(Character.digit('\uFF22', 20)).toEqual(11)
    }

    it("should provide `compareTo`") {
      def compare(x: Char, y: Char): Int =
        new Character(x).compareTo(new Character(y))

      expect(compare('0', '5')).toBeLessThan(0)
      expect(compare('o', 'g')).toBeGreaterThan(0)
      expect(compare('A', 'a')).toBeLessThan(0)
      expect(compare('b', 'b')).toEqual(0)
    }

    it("should be a Comparable") {
      def compare(x: Any, y: Any): Int =
        x.asInstanceOf[Comparable[Any]].compareTo(y)

      expect(compare('0', '5')).toBeLessThan(0)
      expect(compare('o', 'g')).toBeGreaterThan(0)
      expect(compare('A', 'a')).toBeLessThan(0)
      expect(compare('b', 'b')).toEqual(0)
    }

  }
}
