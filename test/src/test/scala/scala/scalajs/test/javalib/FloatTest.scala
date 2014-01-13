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
import scala.scalajs.js.Any.fromInt

object FloatTest extends JasmineTest {

  describe("java.lang.Float") {

    it("should provide proper `equals`") {
      expect(Float.box(0.0f) == Float.box(-0.0f)).toBeFalsy
      expect(Float.box(Float.NaN) == Float.box(Float.NaN)).toBeTruthy
    }

  }
}
