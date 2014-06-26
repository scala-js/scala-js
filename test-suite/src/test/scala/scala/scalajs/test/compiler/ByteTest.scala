/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package compiler

import scala.scalajs.test.JasmineTest
import scala.scalajs.js

object ByteTest extends JasmineTest {

  describe("Byte primitives") {

    it("should always be in their range") {
      def test(x: Int, y: Byte): Unit =
        expect(x.toByte).toEqual(y)

      test(0, 0)
      test(127, 127)
      test(128, -128)
      test(-128, -128)
      test(-500, 12)
      test(-90000, 112)
      test(123456789, 21)
      test(-40000, -64)
      test(65536, 0)
      test(32768, 0)

      def testC(x: Char, y: Byte): Unit =
        expect(x.toByte).toEqual(y)

      testC(-1.toChar, -1)
      testC(200.toChar, -56)
    }

  }
}
