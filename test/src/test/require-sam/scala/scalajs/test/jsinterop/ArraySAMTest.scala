/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package jsinterop

import scala.scalajs.js
import scala.scalajs.test.JasmineTest

object ArraySAMTest extends JasmineTest {

  describe("scala.scalajs.js.Array with SAM support") {

    it("should provide map") {
      expect(js.Array("Sc", "ala", ".", "js").map(_.length)).toEqual(
          js.Array(2, 3, 1, 2))
    }

    it("should provide filter") {
      expect(js.Array(56, 30, -20, 33, 54, 86).filter(_ % 3 != 0)).toEqual(
          js.Array(56, -20, 86))
    }

  }

}
