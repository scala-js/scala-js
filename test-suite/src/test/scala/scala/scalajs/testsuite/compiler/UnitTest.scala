/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.testsuite.compiler

import org.scalajs.jasminetest.JasmineTest
import scala.scalajs.js

object UnitTest extends JasmineTest {

  describe("Unit primitive") {

    it("should have toString()") {
      expect(().toString()).toEqual("undefined")
      expect(((): Any).toString()).toEqual("undefined")
    }

    it("should have hashCode()") {
      expect(().hashCode()).toEqual(0)
      expect(((): Any).hashCode()).toEqual(0)
      expect(().##).toEqual(0)
    }

    it("should equal itself") {
      expect(().equals(())).toBeTruthy
      expect(((): Any).equals((): Any)).toBeTruthy
    }

    it("should not equal other values") {
      def testAgainst(v: Any): Unit = {
        expect(().equals(v)).toBeFalsy
        expect(((): Any).equals(v)).toBeFalsy
      }

      testAgainst(0)
      testAgainst(1)
      testAgainst(null)
      testAgainst(false)
      testAgainst("")
    }

  }
}
