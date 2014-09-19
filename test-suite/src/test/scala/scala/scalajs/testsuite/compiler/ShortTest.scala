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

object ShortTest extends JasmineTest {

  describe("Short primitives") {

    it("should always be in their range") {
      def test(x: Int, y: Short): Unit =
        expect(x.toShort).toEqual(y)

      test(0, 0)
      test(-500, -500)
      test(-90000, -24464)
      test(123456789, -13035)
      test(-40000, 25536)
      test(65536, 0)
      test(32768, -32768)

      def testC(x: Char, y: Short): Unit =
        expect(x.toShort).toEqual(y)

      testC(-1.toChar, -1)
      testC(200.toChar, 200)
      testC(60000.toChar, -5536)
    }

  }
}
