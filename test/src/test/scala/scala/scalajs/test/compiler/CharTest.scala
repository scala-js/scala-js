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
import scala.scalajs.js.Any.fromInt

object CharTest extends JasmineTest {

  describe("Char primitives") {

    it("should always be positive (when coerced)") {
      expect(-3.toByte.toChar.toInt).toEqual(65533)
      expect(-100.toShort.toChar.toInt).toEqual(65436)
      expect(-66000.toChar.toInt).toEqual(65072)
      expect(-4567L.toChar.toInt).toEqual(60969)
      expect(-5.3f.toChar.toInt).toEqual(65531)
      expect(-7.9.toChar.toInt).toEqual(65529)
    }

    it("should overflow (when coerced)") {
      expect(347876543.toChar.toInt).toEqual(11455)
      expect(34234567876543L.toChar.toInt).toEqual(57279)
    }

  }
}
