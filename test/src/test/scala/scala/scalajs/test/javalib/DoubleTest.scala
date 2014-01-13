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

object DoubleTest extends JasmineTest {

  describe("java.lang.Double") {

    it("should provide proper `equals`") {
      expect(Double.box(0.0) == Double.box(-0.0)).toBeFalsy
      expect(Double.box(Double.NaN) == Double.box(Double.NaN)).toBeTruthy
    }

  }
}
