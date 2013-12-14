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

object ArrayTest extends JasmineTest {

  describe("scala.scalajs.js.Array") {

    it("should provide implicit conversion from js.Array to ArrayOps - String") {
      var propCount = 0
      var propString = ""

      for (item <- js.Array("Sc", "ala", ".", "js")) {
        propCount += 1
        propString += item
      }

      expect(propCount).toEqual(4)
      expect(propString).toEqual("Scala.js")
    }

    it("should provide implicit conversion from js.Array to ArrayOps - Int") {
      var propCount = 0
      var propString = ""

      for (item <- js.Array(7, 3, 5, 7)) {
        propCount += 1
        propString += item
      }

      expect(propCount).toEqual(4)
      expect(propString).toEqual("7357")
    }

  }

}
